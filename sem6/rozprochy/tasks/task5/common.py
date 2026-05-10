import pika

EXCHANGE_NAME = "exchange"
SERVICES = set(["human", "cargo", "satelite"])

# Make connection
def make_connection() -> pika.BlockingConnection:
    return pika.BlockingConnection(pika.ConnectionParameters(host="localhost"))

# Setup topic exchange
def setup_exchange(channel: pika.adapters.blocking_connection.BlockingChannel) -> None:
    channel.exchange_declare(exchange=EXCHANGE_NAME, exchange_type="topic")
