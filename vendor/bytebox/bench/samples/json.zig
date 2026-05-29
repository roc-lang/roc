const std = @import("std");

const AllTables = struct {
    const Product = struct {
        id: u64,
        name: []u8,
        weight_grams: u32,
        category: []u8,
        store: u64,
    };

    const Customer = struct {
        id: u64,
        name: []u8,
        address: []u8,
        age: u16,
        family_size: u16,
        bought_item_ids: []u64,
    };

    const Employee = struct {
        id: u64,
        store: u64,
        name: []u8,
        age: u16,
        title: []u8,
        productivity_score: f32,
        manager_id: u64,
    };

    const Store = struct {
        id: u64,
        name: []u8,
        address: []u8,
        phone: []u8,
        manager_id: u64,
    };

    products: []Product,
    customers: []Customer,
    employees: []Employee,
    stores: []Store,

    fn hash(tables: *const AllTables, n: usize) i64 {
        const asBytes = std.mem.asBytes;
        const sliceAsBytes = std.mem.sliceAsBytes;
        const hasher = std.hash.CityHash64.hashWithSeed;

        var v: u64 = n;
        v = hasher(asBytes(tables), v);
        v = hasher(sliceAsBytes(tables.products), v);
        for (tables.products) |entry| {
            v = hasher(asBytes(&entry.id), v);
            v = hasher(entry.name, v);
            v = hasher(asBytes(&entry.weight_grams), v);
            v = hasher(entry.category, v);
            v = hasher(asBytes(&entry.store), v);
        }
        v = hasher(sliceAsBytes(tables.customers), v);
        for (tables.customers) |entry| {
            v = hasher(asBytes(&entry.id), v);
            v = hasher(entry.name, v);
            v = hasher(entry.address, v);
            v = hasher(asBytes(&entry.age), v);
            v = hasher(asBytes(&entry.family_size), v);
            v = hasher(sliceAsBytes(entry.bought_item_ids), v);
        }
        v = hasher(sliceAsBytes(tables.employees), v);
        for (tables.employees) |entry| {
            v = hasher(asBytes(&entry.id), v);
            v = hasher(sliceAsBytes(entry.name), v);
            v = hasher(asBytes(&entry.age), v);
            v = hasher(asBytes(&entry.title), v);
            v = hasher(asBytes(&entry.productivity_score), v);
            v = hasher(asBytes(&entry.manager_id), v);
        }
        v = hasher(sliceAsBytes(tables.stores), v);
        for (tables.stores) |entry| {
            v = hasher(asBytes(&entry.id), v);
            v = hasher(entry.name, v);
            v = hasher(entry.address, v);
            v = hasher(entry.phone, v);
            v = hasher(asBytes(&entry.manager_id), v);
        }

        return @bitCast(v);
    }
};

const alphanumeric = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_ ";

fn randSelect(string: []const u8, rng: *std.Random) u8 {
    const index = rng.uintLessThan(usize, string.len);
    return string[index];
}

fn randString(rng: *std.Random, allocator: std.mem.Allocator) []u8 {
    const length = 8 + rng.uintLessThan(usize, 24);
    const string = allocator.alloc(u8, length) catch @panic("OOM");
    for (string) |*c| {
        c.* = randSelect(alphanumeric, rng);
    }
    return string;
}

fn generate(n: usize, allocator: std.mem.Allocator) AllTables {
    var prng = std.Random.DefaultPrng.init(n);
    var rng = prng.random();

    // generate some json text
    var json_writer: std.Io.Writer.Allocating = .init(allocator);
    defer json_writer.deinit();
    {
        const products = allocator.alloc(AllTables.Product, n) catch @panic("OOM");
        for (products) |*entry| {
            entry.id = rng.int(u64);
            entry.name = randString(&rng, allocator);
            entry.weight_grams = 500 + rng.uintLessThan(u32, 5000);
            entry.category = randString(&rng, allocator);
            entry.store = rng.int(u64);
        }
        const customers = allocator.alloc(AllTables.Customer, n) catch @panic("OOM");
        for (customers) |*entry| {
            entry.id = rng.int(u64);
            entry.name = randString(&rng, allocator);
            entry.address = randString(&rng, allocator);
            entry.age = rng.int(u16);
            entry.family_size = rng.int(u16);
            rng.bytes(std.mem.sliceAsBytes(entry.bought_item_ids));
        }
        const employees = allocator.alloc(AllTables.Employee, n) catch @panic("OOM");
        for (employees) |*entry| {
            entry.id = rng.int(u64);
            entry.store = rng.int(u64);
            entry.name = randString(&rng, allocator);
            entry.age = 16 + rng.uintLessThan(u16, 80);
            entry.title = randString(&rng, allocator);
            entry.productivity_score = rng.float(f32);
            entry.manager_id = rng.int(u64);
        }
        const stores = allocator.alloc(AllTables.Store, n) catch @panic("OOM");
        for (stores) |*entry| {
            entry.id = rng.int(u64);
            entry.name = randString(&rng, allocator);
            entry.phone = randString(&rng, allocator);
            entry.manager_id = rng.int(u64);
        }

        const tables = AllTables{
            .products = products,
            .customers = customers,
            .employees = employees,
            .stores = stores,
        };

        std.json.Stringify.value(tables, .{}, &json_writer.writer) catch @panic("can't serialize json");
    }

    // parse the json text into a native type
    const parsed = std.json.parseFromSlice(AllTables, allocator, json_writer.written(), .{}) catch @panic("bad json");
    return parsed.value;
}

export fn run(n_signed: i32) i64 {
    const n: usize = @intCast(@max(0, n_signed));
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();

    const tables: AllTables = generate(n, allocator);
    return tables.hash(n);
}
