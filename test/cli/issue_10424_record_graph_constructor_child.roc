D : {}

S : { d : D }

r = {}

s : S
s = { d: r }

main! = |_| main!({ ..s, d: r })
