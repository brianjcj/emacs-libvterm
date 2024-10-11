const std = @import("std");

const cflags = .{ "-std=c99", "-Wall", "-Wpedantic", "-fPIC", "-O3" };
const libvterm_files = .{ "encoding.c", "keyboard.c", "mouse.c", "parser.c", "pen.c", "screen.c", "state.c", "unicode.c", "vterm.c" };
// const libvterm_dir = "libs/libvterm-0.3.3/";
const libvterm_dir = "libs/libvterm-mirror/";

const liblogc_dir = "libs/log.c/src/";
const liblogc_files = .{"log.c"};

const vterm_module_files = .{ "vterm-module.c", "utf8.c", "elisp.c" };

fn buildexe(b: *std.Build, name: []const u8, source_file: []const u8, libvterm: *std.Build.Step.Compile) void {
    const exe = b.addExecutable(.{
        .name = name,
        .target = b.host,
    });
    exe.addCSourceFile(.{ .file = b.path(source_file), .flags = &cflags });
    exe.linkLibrary(libvterm);
    exe.addIncludePath(b.path(libvterm_dir ++ "include"));
    b.installArtifact(exe);
}

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const libvterm = b.addStaticLibrary(.{
        .name = "libvterm",
        .target = target,
        .optimize = optimize,
        .version = .{ .major = 1, .minor = 2, .patch = 3 },
    });
    libvterm.addCSourceFiles(.{ .files = &libvterm_files, .root = b.path(libvterm_dir ++ "src"), .flags = &cflags });
    libvterm.addCSourceFiles(.{ .files = &liblogc_files, .root = b.path(liblogc_dir), .flags = &cflags });
    libvterm.addIncludePath(b.path(libvterm_dir ++ "include"));
    libvterm.addIncludePath(b.path(liblogc_dir));
    libvterm.linkLibC();
    b.installArtifact(libvterm);

    // buildexe(b, "vterm-dump", "libs/libvterm-mirror/bin/vterm-dump.c", libvterm);
    // buildexe(b, "unterm", "libs/libvterm-mirror/bin/unterm.c", libvterm);
    // buildexe(b, "vterm-ctrl", "libs/libvterm-mirror/bin/vterm-ctrl.c", libvterm); // this use some unix headers

    const libvterm_module = b.addSharedLibrary(.{
        .name = "vterm-module",
        .target = target,
        .optimize = optimize,
        .version = .{ .major = 1, .minor = 2, .patch = 3 },
    });
    libvterm_module.linkLibrary(libvterm);
    libvterm_module.linkLibC();
    libvterm_module.addCSourceFiles(.{ .files = &vterm_module_files, .root = b.path("."), .flags = &cflags });
    libvterm_module.addIncludePath(b.path(libvterm_dir ++ "include"));
    libvterm_module.addIncludePath(b.path("."));
    libvterm_module.addIncludePath(b.path(liblogc_dir));
    b.installArtifact(libvterm_module);
}
