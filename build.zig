const std = @import("std");

fn readFullDir(
    path: []const u8,
    files: *std.ArrayList([]const u8),
    allocator: std.mem.Allocator,
    ignore_list: []const []const u8,
) !void {
    const cwd = try std.process.getCwdAlloc(allocator);
    const absolute = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ cwd, path });
    defer allocator.free(absolute);

    var dir = try std.fs.openDirAbsolute(absolute, .{ .iterate = true });
    var dir_iter = dir.iterate();

    while (try dir_iter.next()) |entry| switch (entry.kind) {
        .file => {
            if (std.mem.containsAtLeast(u8, entry.name, 1, ".h")) continue;
            var should_ignore = false;
            for (ignore_list) |file| if (std.mem.containsAtLeast(u8, entry.name, 1, file)) {
                should_ignore = true;
            };

            if (should_ignore) continue;

            const file_path = try std.fmt.allocPrint(
                allocator,
                "{s}/{s}",
                .{ path, entry.name },
            );
            try files.append(file_path);
        },
        .directory => {
            const entry_path = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ path, entry.name });
            defer allocator.free(entry_path);
            try readFullDir(entry_path, files, allocator, ignore_list);
        },
        else => {},
    };
}

fn buildCompiler(
    b: *std.Build,
    flags: std.ArrayList([]const u8),
    target: std.Build.ResolvedTarget,
    optimize: std.builtin.OptimizeMode,
    allocator: std.mem.Allocator,
) !*std.Build.Step.Compile {
    const root_module = b.createModule(.{ .target = target, .optimize = optimize });
    const loy = b.addExecutable(.{ .name = "loy", .root_module = root_module });

    var compiler_files = std.ArrayList([]const u8).init(allocator);
    defer compiler_files.deinit();
    try readFullDir("src", &compiler_files, allocator, &[_][]const u8{});

    loy.addCSourceFiles(.{
        .root = b.path("."),
        .files = compiler_files.items,
        .flags = flags.items,
    });

    loy.linkLibC();
    return loy;
}

pub fn buildTests(
    b: *std.Build,
    flags: std.ArrayList([]const u8),
    target: std.Build.ResolvedTarget,
    optimize: std.builtin.OptimizeMode,
    allocator: std.mem.Allocator,
) !*std.Build.Step.Compile {
    const root_module = b.createModule(.{ .target = target, .optimize = optimize });
    const tests = b.addExecutable(.{ .name = "loy_tests", .root_module = root_module });

    var compiler_files = std.ArrayList([]const u8).init(allocator);
    defer compiler_files.deinit();
    try readFullDir("src", &compiler_files, allocator, &[_][]const u8{"main.c"});
    try readFullDir("tests", &compiler_files, allocator, &[_][]const u8{});

    tests.addCSourceFiles(.{
        .root = b.path("."),
        .files = compiler_files.items,
        .flags = flags.items,
    });

    tests.linkLibC();
    return tests;
}

pub fn build(b: *std.Build) !void {
    const enable_valgrind = b.option(
        bool,
        "valgrind",
        "Build with valgrind compatibility",
    ) orelse false;

    const allocator = std.heap.page_allocator;

    var files = std.ArrayList([]const u8).init(allocator);
    defer files.deinit();

    var target_query: std.Target.Query = .{};
    if (enable_valgrind) target_query.cpu_arch = .x86_64;
    const target = b.resolveTargetQuery(target_query);
    const optimize = b.standardOptimizeOption(.{});

    var compiler_flags = std.ArrayList([]const u8).init(allocator);
    defer compiler_flags.deinit();

    try compiler_flags.append("-std=c99");
    try compiler_flags.append("-pedantic");
    try compiler_flags.append("-Wall");
    try compiler_flags.append("-Wextra");
    try compiler_flags.append("-Wconversion");
    try compiler_flags.append("-Wundef");
    try compiler_flags.append("-Werror");

    const loy = try buildCompiler(b, compiler_flags, target, optimize, allocator);
    const tests = try buildTests(b, compiler_flags, target, optimize, allocator);
    b.installArtifact(loy);
    b.installArtifact(tests);

    const run_step = b.step("run", "Run the app");
    const run_cmd = b.addRunArtifact(loy);
    run_step.dependOn(&run_cmd.step);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| run_cmd.addArgs(args);

    const test_step = b.step("test", "Run unit tests");
    const test_cmd = b.addRunArtifact(tests);
    test_step.dependOn(&test_cmd.step);
    test_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| test_cmd.addArgs(args);
}
