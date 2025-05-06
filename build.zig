const std = @import("std");
const Build = std.Build;
const Step = Build.Step;

pub fn build(b: *Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const exe = b.addExecutable(.{
        .name = "lune-lang",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    const compiler_lib = b.addModule("lune", .{
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
    });
    compiler_lib.addImport("lune", compiler_lib);

    // Dependencies
    add_dependencies(b, exe, compiler_lib);

    // Steps
    make_run_step(b, exe);
    make_test_step(b, compiler_lib);
}

fn add_dependencies(b: *Build, exe: *Step.Compile, compiler_lib: *Build.Module) void {
    // const clap = b.dependency("clap", .{});
    // exe.root_module.addImport("clap", clap.module("clap"));

    exe.root_module.addImport("lune", compiler_lib);

    const pretty = b.dependency("pretty", .{});
    exe.root_module.addImport("pretty", pretty.module("pretty"));

    b.installArtifact(exe);
}

fn make_run_step(b: *Build, exe: *Step.Compile) void {
    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);
}

fn make_test_step(b: *Build, compiler_lib: *Build.Module) void {
    const exe_unit_tests = b.addTest(.{
        .root_source_file = b.path("unitests/main.zig"),
    });

    exe_unit_tests.root_module.addImport("lune", compiler_lib);

    const run_exe_unit_tests = b.addRunArtifact(exe_unit_tests);
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_exe_unit_tests.step);
}
