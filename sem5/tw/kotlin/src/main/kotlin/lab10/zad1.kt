package lab10

import kotlinx.coroutines.*
import kotlinx.coroutines.channels.Channel
import kotlin.random.Random

fun main() {
    val PROCESSOR_COUNT = 5
    // Channels: Prod -C1-> Proc1 -C2-> ... -CN-> ProcN -C(N+1)-> Cons
    val channels = List(PROCESSOR_COUNT + 1) { Channel<Int>() }
    runBlocking {
        launch { producer(channels[0]) }
        for (id in 1..PROCESSOR_COUNT) {
            launch { processor(id, channels[id-1], channels[id]) }
        }
        launch { consumer(channels[PROCESSOR_COUNT]) }
    }
}

private suspend fun producer(outChannel: Channel<Int>) {
    var item = 0
    while (true) {
        delay(Random.nextLong(1000, 2000))
        outChannel.send(++item)
        println("Producer produced item $item")
    }
}

private suspend fun processor(id: Int, inChannel: Channel<Int>, outChannel: Channel<Int>) {
    for (item in inChannel) {
        delay(Random.nextLong(200, 500))
        outChannel.send(item)
        println("Processor $id processed item $item")
    }
}

private suspend fun consumer(inChannel: Channel<Int>) {
    for (item in inChannel) {
        delay(Random.nextLong(300, 700))
        println("Consumer consumed item $item")
    }
}
