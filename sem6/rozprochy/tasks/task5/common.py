import pika

EXCHANGE_NAME = "exchange"
SERVICES = set(["human", "cargo", "satelite"])

# Make connection
def make_connection() -> pika.BlockingConnection:
    return pika.BlockingConnection(pika.ConnectionParameters(host="localhost"))

# Setup topic exchange
def setup_exchange(channel: pika.adapters.blocking_connection.BlockingChannel) -> None:
    channel.exchange_declare(exchange=EXCHANGE_NAME, exchange_type="topic")

# Setup service queues
def setup_service_queues(channel: pika.adapters.blocking_connection.BlockingChannel) -> None:
    for service in SERVICES:
        queue_name = f"service.{service}"
        channel.queue_declare(queue=queue_name)
        channel.queue_bind(exchange=EXCHANGE_NAME, queue=queue_name, routing_key=f"orders.{service}")
