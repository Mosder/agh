import sys
import Ice
import Demo

def print_help() -> None:
    print("Available commands:")
    print(" - get [c]ommon|[d]edicated <obj-name>          - get state of an object")
    print(" - set [c]ommon|[d]edicated <obj-name> <state>  - set state of an object")
    print(" - help                                         - print this help menu")

def valid_command(com: list[str]) -> bool:
    # too few arguments (get)
    if len(com) < 3:
        return False
    # undefined action
    if com[0] not in ["get", "set"]:
        return False
    # too few arguments (set)
    if com[0] == "set" and len(com) < 4:
        return False
    # wrong category
    if com[1] not in ["common", "dedicated", "c", "d"]:
        return False

    return True

def extend_category(category: str) -> str:
    if category == "c":
        return "common"
    if category == "d":
        return "dedicated"
    return category

def run_client(args: list[str]) -> None:
    PROXY_TEMPLATE = "{category}/{name}:tcp -h 127.0.0.2 -p 10000 -z : udp -h 127.0.0.2 -p 10000 -z"

    with Ice.initialize(args) as communicator:
        while True:
            command = input("Command (help for help): ")
            com = command.split(" ")
            if len(com) >= 1 and com[0] == "help":
                print_help()
            elif valid_command(com):
                action = com[0]
                category = extend_category(com[1])
                obj_name = com[2]

                base = communicator.stringToProxy(PROXY_TEMPLATE.format(category=category, name=obj_name))
                # use unchecked cast to not create servant on "handshake"
                obj = Demo.ItemPrx.uncheckedCast(base)
                if not obj:
                    print("Couldn't get object")

                if action == "get":
                    state = obj.getState()
                    print(f"State of {category}/{obj_name} - {state}")
                elif action == "set":
                    state = com[3]
                    obj.setState(state)
                    print(f"Set state of {category}/{obj_name} to {state}")
            else:
                print("Command not found")
            print()

if __name__ == "__main__":
    run_client(sys.argv)
