package day14

import java.io.File

typealias Position = Pair<Int, Int>
typealias Velocity = Pair<Int, Int>

data class Robot(var position: Position, val velocity: Velocity) {
    fun move(seconds: Int, height: Int, width: Int) {
        position = position.let {
            Pair(
                Math.floorMod(it.first + velocity.first * seconds, height),
                Math.floorMod(it.second + velocity.second * seconds, width),
            )
        }
    }
}

fun parse(inp: String) = """p=(\d+),(\d+) v=(-?\d+),(-?\d+)""".toRegex().findAll(inp).map { line ->
    val (x, y, dx, dy) = line.groupValues.drop(1).map { it.toInt() }
    Robot(y to x, dy to dx)
}.toList()

fun quadrantsOf(height: Int, width: Int): List<Pair<IntRange, IntRange>> {
    val middleRow = height / 2
    val middleCol = width / 2

    return listOf(
        (0..<middleRow) to (0..<middleCol),
        (0..<middleRow) to (middleCol + 1..<width),
        (middleRow + 1..<height) to (0..<middleCol),
        (middleRow + 1..<height) to (middleCol + 1..<width),
    )
}

fun first(inp: String, height: Int, width: Int): Int {
    val robots = parse(inp)
    robots.forEach { it.move(100, height, width) }

    return quadrantsOf(height, width)
        .map { quadrant ->
            robots.count { robot ->
                robot.position.first in quadrant.first && robot.position.second in quadrant.second
            }
        }
        .reduce { acc, i -> acc * i }
}

fun printRobots(cover: Set<Position>, height: Int, width: Int) {
    for (y in 0..<height) {
        for (x in 0..<width) {
            print(if (cover.contains(y to x)) '#' else '.')
        }
        println()
    }
}

fun second(inp: String, height: Int, width: Int): Int {
    val robots = parse(inp)

    val covers = mutableListOf<MutableSet<Position>>()
    repeat(height * width) {
        val cover = mutableSetOf<Position>()
        robots.forEach {
            it.move(1, height, width)
            cover += it.position
        }
        covers += cover
    }

    val easterEgg = covers.withIndex().maxBy { it.value.size }
    printRobots(easterEgg.value, height, width)

    return easterEgg.index + 1
}

fun main() {
    val testInput = File("src/main/resources/day14/test-input.txt").readText()
    val input = File("src/main/resources/day14/input.txt").readText()
    println(first(input, 103, 101))
    println(second(input, 103, 101))
}
