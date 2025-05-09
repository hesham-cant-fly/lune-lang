const std = @import("std");
const lune = @import("lune");

const TSAST = lune.TSAST;
const Symbol = lune.Symbol;

const Module = @This();

path: []const u8,
tsast: TSAST.Program,
exported_symbol: std.StringHashMap(Symbol),
// exported_user_types: std.StringHashMap(),

pub fn deinit(self: *Module) void {
    self.exported_symbol.deinit();
}
