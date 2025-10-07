//! Generated file - embeds compiled builtin ModuleEnvs
//! DO NOT EDIT - This file is auto-generated at build time

const std = @import("std");
const can = @import("can");
const ModuleEnv = can.ModuleEnv;

/// Serialized ModuleEnv bytes for Bool
pub const Bool_env_bytes = @embedFile("Bool.roc.env");
/// Serialized ModuleEnv bytes for Dict
pub const Dict_env_bytes = @embedFile("Dict.roc.env");
/// Serialized ModuleEnv bytes for Set
pub const Set_env_bytes = @embedFile("Set.roc.env");

/// Load all embedded builtin ModuleEnvs by deserializing them
pub fn loadAll(gpa: std.mem.Allocator) ![]const *const ModuleEnv {
    const compile = @import("compile");
    const CacheModule = compile.CacheModule;

    var envs = std.ArrayList(*const ModuleEnv).init(gpa);
    errdefer {
        for (envs.items) |env| {
            @constCast(env).deinit();
            gpa.destroy(@constCast(env));
        }
        envs.deinit();
    }

    // Deserialize Bool
    {
        const env_ptr = try gpa.create(ModuleEnv);
        errdefer gpa.destroy(env_ptr);
        env_ptr.* = try CacheModule.load(gpa, Bool_env_bytes);
        try envs.append(env_ptr);
    }
    // Deserialize Dict
    {
        const env_ptr = try gpa.create(ModuleEnv);
        errdefer gpa.destroy(env_ptr);
        env_ptr.* = try CacheModule.load(gpa, Dict_env_bytes);
        try envs.append(env_ptr);
    }
    // Deserialize Set
    {
        const env_ptr = try gpa.create(ModuleEnv);
        errdefer gpa.destroy(env_ptr);
        env_ptr.* = try CacheModule.load(gpa, Set_env_bytes);
        try envs.append(env_ptr);
    }

    return envs.toOwnedSlice();
}
