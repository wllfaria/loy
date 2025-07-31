@compile:
    zig build

@run:
    zig build run

@run_debug:
    zig build run -Dmem_debug=true

@valgrind:
    just compile
    valgrind \
        --leak-check=full \
        --track-origins=yes \
        --show-leak-kinds=all \
        --num-callers=15 \
        ./zig-out/bin/loy
