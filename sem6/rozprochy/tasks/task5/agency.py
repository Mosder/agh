import sys
import json
from threading import Thread
from common import EXCHANGE_NAME, make_connection, setup_exchange, setup_service_queues

ALLOWED_ARGS = set(["human", "h", "cargo", "c", "satelite", "s"])
EXPAND_ARGS_DICT = {"h": "human", "c": "cargo", "s": "satelite"}

# Global agency name to be visible in callbacks
agency_name = None

# Print allowed commands
def print_help() -> None:
    print("\nInput service ([h]uman, [c]argo, [s]atelite):")

# Setup confirmation queue
def setup_confirmation_queue(channel: pika.adapters.blocking_connection.BlockingChannel) -> str:
    queue_name = f"confirmations.{agency_name}"
    channel.queue_declare(queue=queue_name)
    channel.queue_bind(exchange=EXCHANGE_NAME, queue=queue_name, routing_key=f"confirm.{agency_name}")
    return queue_name

# Setup admin queue
def setup_admin_queue(channel: pika.adapters.blocking_connection.BlockingChannel) -> str:
    queue_name = f"admin.agencies.{agency_name}"
    channel.queue_declare(queue=queue_name)
    channel.queue_bind(exchange=EXCHANGE_NAME, queue=queue_name, routing_key="admin.agencies")
    channel.queue_bind(exchange=EXCHANGE_NAME, queue=queue_name, routing_key="admin.all")
    return queue_name

# Handle confirmation callbacks
def handle_confirm(ch, method, props, body):
    data = json.loads(body.decode("utf-8"))

    order_id = data["order_id"]
    service = data["service"]
    carrier = data["carrier"]

    print(f"[CARRIER {carrier}]: Confirm order {order_id} for service {service}")
    print_help()
    ch.basic_ack(delivery_tag = method.delivery_tag)

# Handle admin messages
def handle_admin_message(ch, method, props, body):
    data = json.loads(body.decode("utf-8"))
    print(f"[ADMIN]: {data["message"]}")
    print_help()
    ch.basic_ack(delivery_tag = method.delivery_tag)

# Thread for listening
def listener() -> None:
    connection = make_connection()
    channel = connection.channel()
    setup_exchange(channel)
    confirm_queue = setup_confirmation_queue(channel)
    admin_queue = setup_admin_queue(channel)

    channel.basic_consume(queue=confirm_queue, on_message_callback=handle_confirm, auto_ack=False)
    channel.basic_consume(queue=admin_queue, on_message_callback=handle_admin_message, auto_ack=False)
    channel.start_consuming()

# Run agency
def run_agency() -> None:
    # Start thread for listening
    listener_thread = Thread(target=listener, args=(), daemon=True)
    listener_thread.start()

    # Commands thread here
    connection = make_connection()
    channel = connection.channel()
    setup_exchange(channel)
    setup_service_queues(channel)

    print("Started agency")
    order_id = 1
    while True:
        print_help()
        service = input()
        if service not in ALLOWED_ARGS:
            print(f"Unknown service")
            continue
        service = EXPAND_ARGS_DICT.get(service, service)

        message = {
            "agency": agency_name,
            "order_id": order_id,
            "service": service
        }
        channel.basic_publish(
            exchange=EXCHANGE_NAME,
            routing_key=f"orders.{service}",
            body=json.dumps(message)
        )

        print(f"[SENT] order={order_id}, service={service}")
        order_id += 1

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Wrong argument count")
        print("Required arguments: <name>")
        sys.exit()

    agency_name = sys.argv[1]
    run_agency()
