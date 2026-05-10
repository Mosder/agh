import sys
import json
from threading import Thread
from common import EXCHANGE_NAME, make_connection, setup_exchange

ALLOWED_ARGS = set(["agencies", "a", "carriers", "c", "all"])
EXPAND_ARGS_DICT = {"a": "agencies", "c": "carriers"}

# Prints allowed commands
def print_help() -> None:
    print("\nCommand ([a]gencies|[c]arriers|all <msg>):")

# Setup monitoring queue
def setup_monitor_queue(channel: pika.adapters.blocking_connection.BlockingChannel) -> str:
    queue_name = "admin.monitor"
    channel.queue_declare(queue=queue_name)
    channel.queue_bind(exchange=EXCHANGE_NAME, queue=queue_name, routing_key="#")
    return queue_name

# Handle message from monitor
def handle_monitor_message(ch, method, props, body):
    data = json.loads(body.decode("utf-8"))
    print(f"[MONITOR] key={method.routing_key}, body={data}")
    print_help()
    ch.basic_ack(delivery_tag = method.delivery_tag)

# Thread for monitoring all messages
def listener() -> None:
    connection = make_connection()
    channel = connection.channel()
    setup_exchange(channel)
    monitor_queue = setup_monitor_queue(channel)

    channel.basic_consume(queue=monitor_queue, on_message_callback=handle_monitor_message, auto_ack=False)
    channel.start_consuming()

# Run admin
def run_admin() -> None:
    listener_thread = Thread(target=listener, args=(), daemon=True)
    listener_thread.start()

    connection = make_connection()
    channel = connection.channel()
    setup_exchange(channel)

    print("Started admin")
    while True:
        print_help()
        command = input()
        parts = command.split(" ", 1)
        if len(parts) != 2 or parts[0] not in ALLOWED_ARGS:
            print("Command not found")
            continue

        target, message = parts[0], parts[1]
        channel.basic_publish(
            exchange=EXCHANGE_NAME,
            routing_key=f"admin.{EXPAND_ARGS_DICT.get(target, target)}",
            body=json.dumps({"message": message})
        )

if __name__ == "__main__":
    run_admin()
