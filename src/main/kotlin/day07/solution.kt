package day07

import java.io.File

typealias Operator = (Long, Long) -> Long

data class Equation(val testValue: Long, val numbers: List<Long>) {
    fun canCalibrate(operators: List<Operator>): Boolean {
        fun runCalibration(intermediateResult: Long, remainingNumbers: List<Long>): Boolean {
            if (remainingNumbers.isEmpty()) {
                return intermediateResult == testValue
            }

            return operators
                .map { it(intermediateResult, remainingNumbers.first()) }
                .any { runCalibration(it, remainingNumbers.drop(1)) }
        }

        return runCalibration(numbers.first(), numbers.drop(1))
    }
}

fun parse(inp: String): List<Equation> = inp.lines().map { line ->
    line.split(": ").let { (testValue, numbers) ->
        Equation(
            testValue.toLong(),
            numbers.split(" ").map { it.toLong() }
        )
    }
}

fun first(inp: String): Long {
    return parse(inp)
        .filter { it.canCalibrate(listOf({ a, b -> a + b }, { a, b -> a * b })) }
        .sumOf { it.testValue }
}

fun second(inp: String): Long {
    return parse(inp)
        .filter { it.canCalibrate(listOf({ a, b -> a + b }, { a, b -> a * b }, { a, b -> "$a$b".toLong() })) }
        .sumOf { it.testValue }
}

fun main() {
    val testInput = File("src/main/resources/day07/test-input.txt").readText()
    val input = File("src/main/resources/day07/input.txt").readText()
    println(first(input))
    println(second(input))
}
