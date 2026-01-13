package lab10

import kotlinx.coroutines.*
import kotlinx.coroutines.channels.Channel
import kotlinx.coroutines.selects.selectUnbiased
import kotlin.random.Random

fun main() {
    val MIDDLEMAN_COUNT = 3
    // Channels informing producer that a middleman is waiting
    val waitingChannels = List(MIDDLEMAN_COUNT) { Channel<Unit>() }
    // Channels for sending item from producer to middlemen
    val itemChannels = List(MIDDLEMAN_COUNT) { Channel<Int>() }
    // Channel from which consumer consumes
    val consumerChannel = Channel<Int>()
    runBlocking {
        launch { producer(waitingChannels, itemChannels) }
        for (id in 0..<MIDDLEMAN_COUNT) {
            launch { middleman(id+1, waitingChannels[id], itemChannels[id], consumerChannel) }
        }
        launch { consumer(consumerChannel) }
    }
}

private suspend fun producer(waitingChannels: List<Channel<Unit>>, itemChannels: List<Channel<Int>>) {
    var item = 0
    while (true) {
        delay(Random.nextLong(300, 500))
        selectUnbiased {
            for ((i, channel) in waitingChannels.withIndex()) {
                channel.onReceive {
                    itemChannels[i].send(++item)
                    println("Producer sent item $item to middleman ${i+1}")
                }
            }
        }
    }
}

private suspend fun middleman(id: Int, waitingChannel: Channel<Unit>,
                              itemChannel: Channel<Int>, consumerChannel: Channel<Int>) {
    waitingChannel.send(Unit)
    for (item in itemChannel) {
        delay(Random.nextLong(700, 900))
        consumerChannel.send(item)
        println("Middleman $id sent item $item to consumer")
        waitingChannel.send(Unit)
    }
}

private suspend fun consumer(consumerChannel: Channel<Int>) {
    for (item in consumerChannel) {
        delay(Random.nextLong(100, 400))
        println("Consumer consumed item $item")
    }
}
