package day17

import java.io.File

data class Computer(var a: Int, var b: Int, var c: Int, val program: List<Int>) {
    private fun combo(operand: Int) = when (operand) {
        in 0..3 -> operand
        4 -> a
        5 -> b
        6 -> c
        else -> throw AssertionError("unknown operand $operand")
    }

    fun run(): List<Int> {
        val outputs = mutableListOf<Int>()
        var pointer = 0
        while (true) {
            val opcode = program.getOrNull(pointer) ?: break
            val operand = program[pointer + 1]
            when (opcode) {
                0 -> a = a shr combo(operand)
                1 -> b = b xor operand
                2 -> b = combo(operand) % 8
                3 -> if (a != 0) {
                    pointer = operand
                    continue
                }

                4 -> b = b xor c
                5 -> outputs += combo(operand) % 8
                6 -> b = a shr combo(operand)
                7 -> c = a shr combo(operand)
            }

            pointer += 2
        }

        return outputs
    }
}

fun parse(inp: String): Computer {
    val (registers, program) = inp.split("\n\n")
    val a = registers.lines().first().takeLastWhile { it.isDigit() }.toInt()
    return Computer(a, 0, 0, program = program.split(" ").last().split(",").map { it.toInt() })
}

fun first(inp: String) = parse(inp).run().joinToString(",")

fun second(inp: String): Int {
    // Program: 2,4,1,3,7,5,4,2,0,3,1,5,5,5,3,0
    // 2,4: b = a % 8
    // 1,3: b = a ^ 3
    // 7,5: c = a >> b
    // 4,2: b = b ^ c
    // 0,3: a = a >> 3
    // 1,5: b = a ^ 5
    // 5,5: out(b % 8)
    // 3,0: a != 0 -> jump(0)

    throw NotImplementedError()
}

fun main() {
    val testInput = File("src/main/resources/day17/test-input.txt").readText()
    val testInput2 = File("src/main/resources/day17/test-input2.txt").readText()
    val input = File("src/main/resources/day17/input.txt").readText()
    println(first(input))
    println(second(input))
}
