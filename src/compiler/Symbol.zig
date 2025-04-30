const std = @import("std");
const mem = std.mem;
const root = @import("root");
const TokenKind = root.TokenKind;
const Token = root.Token;
const Type = root.Type;

const Symbol = @This();

pub const Scope = std.StringHashMapUnmanaged(Symbol);
pub const ComptimeValue = i32;

pub const SymbolTable = struct {
    pub const Error = error{
        ReassignmentToConstant,
        UndefinedVariable,
        RedefinitionOfVariable,

        AssignmentToType,
    } || mem.Allocator.Error;

    const Builtin = std.StringHashMapUnmanaged(Symbol);

    allocator: mem.Allocator,
    builtin: Builtin,
    scopes: std.DoublyLinkedList(Scope),

    pub fn init(allocator: mem.Allocator) mem.Allocator.Error!SymbolTable {
        var scopes = std.DoublyLinkedList(Scope){};
        const global = try allocator.create(std.DoublyLinkedList(Scope).Node);
        global.* = std.DoublyLinkedList(Scope).Node{
            .data = Scope{},
        };
        scopes.append(global);
        return SymbolTable{
            .allocator = allocator,
            .builtin = try create_builtins(allocator),
            .scopes = scopes,
        };
    }

    fn create_builtins(allocator: mem.Allocator) mem.Allocator.Error!Builtin {
        var res = Builtin{};

        try res.put(
            allocator,
            "number",
            Symbol.init_type("number", Type.init(.{ .Primitive = .Number })),
        );
        try res.put(
            allocator,
            "string",
            Symbol.init_type("string", Type.init(.{ .Primitive = .String })),
        );
        try res.put(
            allocator,
            "auto",
            Symbol.init_type("auto", Type.init(.Auto)),
        );

        return res;
    }

    pub fn deinit(self: *SymbolTable) void {
        while (self.scopes.pop()) |scope| {
            scope.data.deinit(self.allocator);
            self.allocator.destroy(scope);
        }
        self.builtin.deinit(self.allocator);
    }

    pub fn scope_start(self: *SymbolTable) Error!*Scope {
        const scope = try self.allocator.create(std.DoublyLinkedList(Scope).Node);
        scope.* = std.DoublyLinkedList(Scope).Node{
            .data = Scope{},
        };
        self.scopes.append(scope);
    }

    pub fn scope_end(self: *SymbolTable) void {
        const node = self.scopes.pop() orelse @panic("No scope are defined.");
        node.data.deinit(self.allocator);
        self.allocator.free(node);
    }

    pub fn define_type(self: *SymbolTable, name: Token, tp: Type) Error!void {
        const scope = self.get_current_scope();
        if (scope.get(name.lexem) != null) return Error.RedefinitionOfVariable;
        var symbol = Symbol.init(name, tp);
        symbol.is_type = true;
        try scope.put(self.allocator, name.lexem, symbol);
    }

    pub fn define(self: *SymbolTable, name: Token) Error!void {
        const scope = self.get_current_scope();
        if (scope.get(name.lexem) != null) return Error.RedefinitionOfVariable;
        try scope.put(self.allocator, name.lexem, Symbol.init_unknown(name));
    }

    pub fn define_constant(self: *SymbolTable, name: Token) Error!void {
        const scope = self.get_current_scope();
        if (scope.get(name.lexem) != null) return Error.RedefinitionOfVariable;
        var symbol = Symbol.init_unknown(name);
        symbol.constant = true;
        try scope.put(self.allocator, name.lexem, symbol);
    }

    pub fn declare(self: *SymbolTable, name: Token, tp: Type) void {
        const scope = self.get_current_scope();
        if (scope.getPtr(name.lexem)) |vr| {
            vr.decalred = true;
            vr.value_type = tp;
            return;
        }
        @panic("Declaring undefined variable");
    }

    pub fn assign(self: *SymbolTable, name: []const u8, comptime_value: ?ComptimeValue) Error!void {
        const scope = self.get_current_scope();
        if (scope.getPtr(name)) |vr| {
            if (vr.is_type) return Error.AssignmentToType;
            if (vr.constant and vr.assigned)
                return Error.ReassignmentToConstant;

            vr.comptime_value = comptime_value;
            vr.assigned = true;
            return;
        }
        return Error.UndefinedVariable;
    }

    pub fn get_local_first(self: *SymbolTable, name: []const u8) ?*Symbol {
        var current = self.scopes.last;

        while (current) |node| {
            const scope = node.data;
            if (scope.getPtr(name)) |symbol| {
                return symbol;
            }
            current = node.prev;
        }

        return self.builtin.getPtr(name);
    }

    pub fn get_global(self: *SymbolTable, name: []const u8) ?*Symbol {
        const scope = self.get_global_scope();
        return scope.getPtr(name.lexem);
    }

    pub fn get_type(self: *SymbolTable, name: []const u8) ?*Symbol {
        var current = self.scopes.last;

        while (current) |node| {
            const scope = node.data;
            if (scope.getPtr(name)) |symbol| {
                if (symbol.is_type)
                    return symbol;
            }
            current = node.prev;
        }

        const symbol = self.builtin.getPtr(name) orelse return null;
        if (symbol.is_type)
            return symbol;
        return null;
    }

    pub fn get_global_scope(self: *SymbolTable) *Scope {
        const head = self.scopes.first orelse @panic("Expected a global scope to be present.");
        return head.data;
    }

    pub fn get_current_scope(self: *SymbolTable) *Scope {
        const tail = self.scopes.last orelse @panic("Expected a scope to be present.");
        return &tail.data;
    }
};

name: Token,
value_type: Type,
comptime_value: ?ComptimeValue = null,
is_type: bool = false,
decalred: bool = false,
constant: bool = false,
assigned: bool = false,

pub fn init(name: Token, value_type: Type) Symbol {
    return Symbol{
        .name = name,
        .value_type = value_type,
        .comptime_value = null,
    };
}

pub fn init_unknown(name: Token) Symbol {
    return Symbol{
        .name = name,
        .value_type = .{ .kind = .Auto },
        .comptime_value = null,
    };
}

pub fn init_type(name: []const u8, tp: Type) Symbol {
    return Symbol{
        .name = .{
            .kind = .{ .Identifier = name },
            .lexem = name,
            .column = 0,
            .line = 0,
        },
        .value_type = tp,
        .is_type = true,
        .constant = true,
    };
}
