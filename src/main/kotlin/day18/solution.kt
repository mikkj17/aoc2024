package day18

import java.io.File

typealias Position = Pair<Int, Int>
typealias Direction = Pair<Int, Int>

fun Position.move(direction: Direction) = first + direction.first to second + direction.second

fun parse(inp: String) = inp.lines().map { line -> line.split(",").map { it.toInt() }.let { it.last() to it.first() } }

val directions = listOf(
    -1 to 0,
    1 to 0,
    0 to 1,
    0 to -1,
)

fun neighbours(position: Position, size: Int, bytes: List<Position>) = directions
    .map { position.move(it) }
    .filter { it.first in 0..size && it.second in 0..size }
    .filter { it !in bytes }

fun search(size: Int, bytes: List<Position>): Map<Position, Int> {
    val start = 0 to 0
    val costs = mutableMapOf(start to 0)
    val queue = mutableListOf(start)
    while (queue.isNotEmpty()) {
        val v = queue.removeFirst()
        for (w in neighbours(v, size, bytes)) {
            if (w !in costs) {
                costs += w to costs.getValue(v) + 1
                queue += w
            }
        }
    }

    return costs
}

fun first(inp: String, size: Int, bytesFallen: Int): Int {
    val bytes = parse(inp).take(bytesFallen)
    return search(size, bytes).getValue(size to size)
}

fun second(inp: String, size: Int): String {
    val bytes = parse(inp)
    for (i in bytes.size downTo 1) {
        val costs = search(size, bytes.take(i))
        if (size to size in costs) {
            return bytes[i].let { "${it.second},${it.first}" }
        }
    }

    throw AssertionError()
}

fun main() {
    val testInput = File("src/main/resources/day18/test-input.txt").readText()
    val input = File("src/main/resources/day18/input.txt").readText()
    println(first(input, size = 70, bytesFallen = 1024))
    println(second(input, size = 70))
}
