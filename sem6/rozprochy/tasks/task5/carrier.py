import sys
import pika
import json
from time import sleep
from common import EXCHANGE_NAME, make_connection, setup_exchange, setup_service_queues

HUMAN_TRANSPORT_TIME_S = 0
CARGO_TRANSPORT_TIME_S = 0
PUT_SATELITE_TIME_S = 0

ALLOWED_ARGS = set(["human", "h", "cargo", "c", "satelite", "s"])
EXPAND_ARGS_DICT = {"h": "human", "c": "cargo", "s": "satelite"}
SERVICE_TO_TIME_S = {
    "human": HUMAN_TRANSPORT_TIME_S,
    "cargo": CARGO_TRANSPORT_TIME_S,
    "satelite": PUT_SATELITE_TIME_S
}

# Global carrier name to have it in callback
carrier_name = None

# Create set containing services
def create_services_set(arg1: str, arg2: str) -> set[str]:
    services = set()
    services.add(EXPAND_ARGS_DICT.get(arg1, arg1))
    services.add(EXPAND_ARGS_DICT.get(arg2, arg2))
    return services

# Setup queue for messages from admin
def setup_admin_queue(channel: pika.adapters.blocking_connection.BlockingChannel, name: str) -> str:
    queue_name = f"admin.carriers.{name}"
    channel.queue_declare(queue=queue_name)
    channel.queue_bind(exchange=EXCHANGE_NAME, queue=queue_name, routing_key="admin.carriers")
    channel.queue_bind(exchange=EXCHANGE_NAME, queue=queue_name, routing_key="admin.all")
    return queue_name

# Callback for handling orders
def handle_order(ch, method, props, body):
    data = json.loads(body.decode("utf-8"))

    agency = data["agency"]
    order_id = data["order_id"]
    service = data["service"]

    print(f"[AGENCY {agency}]: order_id={order_id}, service={service}")
    sleep(SERVICE_TO_TIME_S[service])

    confirmation = {
        "order_id": order_id,
        "service": service,
        "carrier": carrier_name
    }
    ch.basic_publish(
        exchange=EXCHANGE_NAME,
        routing_key=f"confirm.{agency}",
        body=json.dumps(confirmation)
    )

    print(f"Handled order {order_id} from agency {agency} and sent confirmation")
    ch.basic_ack(delivery_tag = method.delivery_tag)

# Callback for handling admin messages
def handle_admin_message(ch, method, props, body):
    data = json.loads(body.decode("utf-8"))
    print(f"[ADMIN]: {data["message"]}")
    ch.basic_ack(delivery_tag = method.delivery_tag)

# Run carrier
def run_carrier(services: set[str]) -> None:
    connection = make_connection()
    channel = connection.channel()
    channel.basic_qos(prefetch_count=1)

    setup_exchange(channel)
    setup_service_queues(channel)
    admin_queue = setup_admin_queue(channel, carrier_name)

    for service in services:
        channel.basic_consume(queue=f"service.{service}", on_message_callback=handle_order, auto_ack=False)
    channel.basic_consume(queue=admin_queue, on_message_callback=handle_admin_message, auto_ack=False)

    print(f"Started carrier")
    channel.start_consuming()


if __name__ == "__main__":
    if len(sys.argv) != 4:
        print("Wrong argument count")
        print("Arguments: <name> <service1> <service2>")
        sys.exit()

    if sys.argv[2] not in ALLOWED_ARGS or sys.argv[3] not in ALLOWED_ARGS:
        print("Not allowed arguments provided")
        print("Allowed arguments: [h]uman, [c]argo, [s]atelite")
        sys.exit()

    services = create_services_set(sys.argv[2], sys.argv[3])
    if len(services) < 2:
        print("Carrier must provide two DIFFERENT services")
        sys.exit()

    carrier_name = sys.argv[1]
    run_carrier(services)
