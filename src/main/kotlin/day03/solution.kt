package day03

import java.io.File

val mulPattern = Regex("""mul\((\d+),(\d+)\)""")

fun first(inp: String) = mulPattern
    .findAll(inp)
    .sumOf { it.groupValues[1].toInt() * it.groupValues[2].toInt() }

fun second(inp: String): Int {
    var total = 0
    var enabled = true
    Regex("""$mulPattern|(do\(\))|(don't\(\))""").findAll(inp).forEach { instruction ->
        when (instruction.value) {
            "do()" -> {
                enabled = true
            }

            "don't()" -> {
                enabled = false
            }

            else -> {
                if (enabled)
                    mulPattern.matchEntire(instruction.value)!!.let {
                        total += it.groupValues[1].toInt() * it.groupValues[2].toInt()
                    }
            }
        }
    }

    return total
}

fun main() {
    val testInput = File("src/main/kotlin/day03/test-input.txt").readText()
    val testInput2 = File("src/main/kotlin/day03/test-input2.txt").readText()
    val input = File("src/main/kotlin/day03/input.txt").readText()
    println(first(input))
    println(second(input))
}
