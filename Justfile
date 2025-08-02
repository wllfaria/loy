@compile args:
    zig build {{args}}

@run:
    zig build run

@run_debug:
    zig build run -Dmem_debug=true

@test:
    zig build test

@valgrind path:
    valgrind \
        --leak-check=full \
        --track-origins=yes \
        --show-leak-kinds=all \
        --num-callers=15 \
        {{path}}
        

@run_valgrind:
    just compile -Dvalgrind=true
    just valgrind ./zig-out/bin/loy

@test_valgrind:
    just compile -Dvalgrind=true
    just valgrind ./zig-out/bin/loy_tests
