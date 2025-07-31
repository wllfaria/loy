const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.resolveTargetQuery(.{
        .cpu_arch = .x86_64,
        .os_tag = .linux,
        .abi = .gnu,
    });
    const optimize = b.standardOptimizeOption(.{});

    const loy = b.addExecutable(.{
        .name = "loy",
        .root_module = b.createModule(.{
            .target = target,
            .optimize = optimize,
        }),
    });

    const loy_compiler_files = &.{
        "string/string_builder.c",
        "string/string_slice.c",
        "unicode/ascii.c",
        "vector.c",
        "lexer.c",
        "parser.c",
        "main.c",
    };

    const loy_compiler_flags = &.{
        "-std=c99",
        "-pedantic",
        "-Wall",
        "-Wextra",
        "-Wconversion",
        "-Wundef",
        "-Werror",
    };

    loy.addCSourceFiles(.{
        .files = loy_compiler_files,
        .flags = loy_compiler_flags,
        .root = b.path("src"),
    });

    loy.linkLibC();
    b.installArtifact(loy);

    const run_step = b.step("run", "Run the app");
    const run_cmd = b.addRunArtifact(loy);
    run_step.dependOn(&run_cmd.step);
    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }
}
