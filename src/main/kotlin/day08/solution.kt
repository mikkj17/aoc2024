package day08

import java.io.File

typealias Position = Pair<Int, Int>

data class Antenna(val frequency: Char, val position: Position) {
    operator fun minus(other: Antenna) = Pair(
        position.first - other.position.first,
        position.second - other.position.second,
    )
}

fun parse(inp: String): List<Antenna> {
    val antennas = mutableListOf<Antenna>()

    for ((y, row) in inp.lines().withIndex()) {
        for ((x, elem) in row.withIndex()) {
            if (elem != '.') {
                antennas.add(Antenna(elem, y to x))
            }
        }
    }

    return antennas
}

fun compute(inp: String, createAntinodes: (Antenna, Antenna) -> Set<Position>): Int {
    val height = inp.lines().size
    val width = inp.lines().first().length
    val antennas = parse(inp)

    return antennas.flatMap { antenna ->
        antennas
            .filter { it.frequency == antenna.frequency && it.position != antenna.position }
            .map { createAntinodes(antenna, it) }
    }
        .reduce { acc, positions -> acc + positions }
        .filter { it.first in 0..<height && it.second in 0..<width }
        .size
}

fun first(inp: String) = compute(inp) { antenna, other ->
    setOf(
        (antenna - other).let { antenna.position.first + it.first to antenna.position.second + it.second }
    )
}

fun second(inp: String): Int {
    val height = inp.lines().size
    val width = inp.lines().first().length

    return compute(inp) { antenna, other ->
        val antinodes = mutableSetOf<Position>()
        val distance = antenna - other
        var pos = antenna.position
        while (pos.first in 0..<height && pos.second in 0..<width) {
            antinodes.add(pos)
            pos = pos.let { it.first + distance.first to it.second + distance.second }
        }
        antinodes.toSet()
    }
}

fun main() {
    val testInput = File("src/main/resources/day08/test-input.txt").readText()
    val input = File("src/main/resources/day08/input.txt").readText()
    println(first(input))
    println(second(input))
}
