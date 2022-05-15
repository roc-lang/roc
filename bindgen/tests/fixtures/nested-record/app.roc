app "app"
    packages { pf: "platform" }
    imports []
    provides [ main ] to pf

main = { x: { a: 5, b: 24 }, y: "foo", z: [ 1, 2 ] }
