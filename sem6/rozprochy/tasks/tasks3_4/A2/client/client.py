import grpc
import threading
import gen.steam_sales_pb2 as gen
import gen.steam_sales_pb2_grpc as gen_grpc
from time import sleep

class Tags:
    def __init__(self):
        self.tags = set()
        self.lock = threading.Lock()

    def add(self, new_tags: list[str]) -> None:
        with self.lock:
            self.tags.update(new_tags)

    def remove(self, removed_tags: list[str]) -> None:
        with self.lock:
            self.tags.difference_update(removed_tags)

    def get(self) -> list[str]:
        with self.lock:
            return list(self.tags)

def print_notification(notification: gen.SaleNotification, lock: threading.Lock) -> None:
    with lock:
        print("Received notification:")
        print(f"Game: {notification.game_title}")
        print(f"Game tags: {notification.game_tags}")
        print("Sale info:")
        print(f" - Original price: {notification.sale_information.original_price}")
        print(f" - Sale price: {notification.sale_information.sale_price}")
        print(f" - Discount: {notification.sale_information.discount_percent}%")
        print(f"ProtonDB rank: {gen.ProtonDbRank.Name(notification.protondb_rank)}")
        print("\nInput command (help for help):")

def print_help(lock: threading.Lock) -> None:
    with lock:
        print("Available commands:")
        print(" - add <comma-seperated-tags>     - subscribe to given tags")
        print(" - remove <comma-seperated-tags>  - unsubscribe from given tags")
        print(" - unsubscribe                    - unsubscribe from everything and exit")
        print(" - help                           - print this help")
        print("\nInput command (help for help):")

def print_message(message: str, lock: threading.Lock) -> None:
    with lock:
        print(message)
        print("\nInput command (help for help):")

def handle_command(id: str, tags: Tags, command: str, lock: threading.Lock, stub: gen_grpc.SalesSubscriptionServiceStub) -> bool:
    com = command.split(" ")
    if com[0] == "add":
        if len(com) < 2:
            print_message("No tags specified", lock)
        else:
            new_tags = com[1].split(",")
            tags.add(new_tags)
            stub.AddTags(gen.RequestData(client_id=id, tags=new_tags))
            print_message("Subscribed to given tags", lock)
    elif com[0] == "remove":
        if len(com) < 2:
            print_message("No tags specified", lock)
        else:
            removed_tags = com[1].split(",")
            tags.remove(removed_tags)
            stub.RemoveTags(gen.RequestData(client_id=id, tags=removed_tags))
            print_message("Unsubscribed from given tags", lock)
    elif com[0] == "unsubscribe":
        stub.Unsubscribe(gen.UnsubscribeRequestData(client_id=id))
        exit()
    elif com[0] == "help":
        print_help(lock)
    else:
        print_message("Unrecognized command", lock)

def notification_thread(stub: gen_grpc.SalesSubscriptionServiceStub, lock: threading.Lock, id: str, tags: Tags) -> None:
    backoff = 1
    while True:
        request = gen.RequestData(client_id=id, tags=tags.get())
        subscribe_stream = stub.Subscribe(request)
        try:
            for notification in subscribe_stream:
                print_notification(notification, lock)
                backoff = 1
        except grpc.RpcError as e:
            if e.code() == grpc.StatusCode.UNAVAILABLE or e.code() == grpc.StatusCode.DEADLINE_EXCEEDED:
                sleep(backoff)
                backoff *= 2
            else:
                break

def run_client(id: str, tags: Tags, server: str = "127.0.0.1:50051") -> None:
    output_lock = threading.Lock()
    options = [
        ("grpc.keepalive_time_ms", 20000),
        ("grpc.keepalive_timeout_ms", 10000),
        ("grpc.keepalive_permit_without_calls", 1),
    ]

    with grpc.insecure_channel(server, options=options) as channel:
        try:
            grpc.channel_ready_future(channel).result(timeout=3)
        except:
            print("Failed to connect to server")
            return

        stub = gen_grpc.SalesSubscriptionServiceStub(channel)
        print_message("Connected to server", output_lock)
        listener = threading.Thread(target=notification_thread, args=(stub, output_lock, id, tags))
        listener.start()

        while True:
            command = input()
            handle_command(id, tags, command, output_lock, stub)


if __name__ == "__main__":
    name = input("Input ID: ")
    tags = Tags()
    while (inp := input("Input a tag (q to continue): ")) != "q":
        tags.add([inp])
    run_client(name, tags)

