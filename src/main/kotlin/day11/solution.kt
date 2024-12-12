package day11

import java.io.File

fun first(inp: String, blinks: Int = 25): Int {
    var stones = inp.split(" ").map { it.toLong() }

    repeat(blinks) {
        stones = stones.flatMap { stone ->
            val stringed = stone.toString()
            when {
                stone == 0L -> listOf(1)
                stringed.length % 2 == 0 -> listOf(
                    stringed.take(stringed.length / 2).toLong(),
                    stringed.drop(stringed.length / 2).toLong(),
                )

                else -> listOf(stone * 2024)
            }
        }
    }

    return stones.size
}

fun second(inp: String, blinks: Int = 75): Long {
    val stones = inp.split(" ").map { it.toLong() }
    val cache = mutableMapOf<Pair<Int, Long>, Long>()

    fun count(stone: Long, blink: Int): Long {
        if (blink == blinks) {
            return 1
        }

        return cache.getOrPut(blink to stone) {
            val stringed = stone.toString()
            val middle = stringed.length / 2
            when {
                stone == 0L -> count(1, blink + 1)
                stringed.length % 2 == 0 -> stringed.chunked(middle)
                    .sumOf { count(it.toLong(), blink + 1) }

                else -> count(stone * 2024, blink + 1)
            }
        }
    }

    return stones.sumOf { count(it, 0) }
}

fun main() {
    val testInput = File("src/main/resources/day11/test-input.txt").readText()
    val input = File("src/main/resources/day11/input.txt").readText()
    println(first(input))
    println(second(input))
}
