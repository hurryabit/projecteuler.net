// Problem467/main.swift

func digitalRoot(var n: Int) -> Int {
    var s = 0
    while n > 0 {
        s += n % 10
        n /= 10
    }
    return s < 10 ? s : digitalRoot(s)
}

var primes = [Bool](count: 105000, repeatedValue: true)

primes[0] = false
primes[1] = false
for p in 2 ..< primes.count {
    if primes[p] {
        for var n = p*p; n < primes.count; n += p {
            primes[n] = false
        }
    }
}

println("got primes")

let size = 1000
var p10000 = [Int]()
var c10000 = [Int]()

for var n = 2; p10000.count < size || c10000.count < size; ++n {
    if primes[n] && p10000.count < size {
        p10000.append(digitalRoot(n))
    }
    if !primes[n] && c10000.count < size {
        c10000.append(digitalRoot(n))
    }
}

println("got numbers")

var table: [[Int]] = Array(count: size+1, repeatedValue: Array(count: size+1, repeatedValue: 0))

for var i = size-1; i >= 0; --i {
    for var j = size-1; j >= 0; --j {
        table[i][j] = p10000[i] == c10000[j] ? table[i+1][j+1] + 1 : max(table[i+1][j],table[i][j+1])
    }
}

println("got table")

var result = 0

func addDigit(d: Int) {
    result = (10*result+d) % 1000000007
}

var i = 0, j = 0

while i < size && j < size {
    if p10000[i] == c10000[j] {
        addDigit(p10000[i])
        ++i
        ++j
    }
    else if table[i][j+1] < table[i][j] || (table[i+1][j] == table[i][j] && p10000[i] < c10000[j]) {
        addDigit(p10000[i])
        ++i
    }
    else if table[i+1][j] < table[i][j] || (table[i][j+1] == table[i][j] && p10000[i] > c10000[j]) {
        addDigit(c10000[j])
        ++j
    }
    else {
        println("ERROR!")
    }
}

for ; i < size; ++i {
    addDigit(p10000[i])
}
for ; j < size; ++j {
    addDigit(c10000[j])
}

println("got result \(result)")
