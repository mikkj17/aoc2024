package day13

import java.io.File
import kotlin.math.floor

val buttonPattern = """Button [AB]: X\+(\d+), Y\+(\d+)""".toRegex()
val prizePattern = """Prize: X=(\d+), Y=(\d+)""".toRegex()

data class Machine(val a: Button, val b: Button, val prize: Prize)

data class Button(val x: Long, val y: Long)

data class Prize(val x: Long, val y: Long)

fun parse(inp: String, toAdd: Long) = inp.split("\n\n").map { machine ->
    val (a, b) = buttonPattern.findAll(machine).toList().map { button ->
        button.groupValues.drop(1)
            .map { it.toLong() }
            .let { Button(it.first(), it.last()) }
    }
    val prize = prizePattern.find(machine)!!.groupValues.drop(1)
        .map { it.toLong() }
        .let { Prize(it.first() + toAdd, it.last() + toAdd) }

    Machine(a, b, prize)
}

fun first(inp: String) = parse(inp, 0).sumOf { machine ->
    val possibleCombinations = mutableListOf<Pair<Int, Int>>()

    for (a in (1..100)) {
        for (b in 1..100) {
            if (a * machine.a.x + b * machine.b.x == machine.prize.x) {
                if (a * machine.a.y + b * machine.b.y == machine.prize.y) {
                    possibleCombinations.add(a to b)
                }
            }
        }
    }

    if (possibleCombinations.isEmpty()) 0
    else possibleCombinations.minOf { (a, b) -> a * 3 + b }
}

fun second(inp: String) = parse(inp, 10000000000000).map { machine ->
    // Is system of linear equations:
    // ax * a + bx * b = px
    // ay * a + by * b = py
    // Rewrite into:
    // A = (px*by - py*bx)/(ax*by - ay*bx)
    // B = (px - ax*A)/(bx)
    (machine.prize.x * machine.b.y - machine.prize.y * machine.b.x)
        .div((machine.a.x * machine.b.y - machine.a.y * machine.b.x).toDouble())
        .let {
            it to (machine.prize.x - machine.a.x * it) / machine.b.x
        }
}
    .filter { (a, b) -> floor(a) == a && floor(b) == b }
    .sumOf { (it.first * 3 + it.second).toLong() }

fun main() {
    val testInput = File("src/main/resources/day13/test-input.txt").readText()
    val input = File("src/main/resources/day13/input.txt").readText()
    println(first(input))
    println(second(input))
}
