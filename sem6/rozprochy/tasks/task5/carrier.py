import sys

HUMAN_TRANSPORT_TIME_S = 1
CARGO_TRANSPORT_TIME_S = 1
PUT_SATELITE_TIME_S = 1
ALLOWED_ARGS = ["human", "h", "cargo", "c", "satelite", "s"]
EXPAND_ARGS_DICT = {"h": "human", "c": "cargo", "s": "satelite"}

def create_services_set(arg1, arg2):
    self.services = set()
    self.services.add(EXPAND_ARGS_DICT.get(arg1, arg1))
    self.services.add(EXPAND_ARGS_DICT.get(arg2, arg2))

def run_carrier(services):
    pass

if __name__ == "__main__":
    if len(sys.argv) != 3:
        print("Wrong argument count")
        print("Arguments: <service1> <service2>")
        sys.exit()

    if sys.argv[1] not in ALLOWED_ARGS or sys.argv[2] not in ALLOWED_ARGS:
        print("Not allowed arguments provided")
        print("Allowed arguments: [h]uman, [c]argo, [s]atelite")
        sys.exit()

    services = create_services_set(sys.argv[1], sys.argv[2])
    if len(services) < 2:
        print("Carrier must provide two DIFFERENT services")
        sys.exit()

    run_carrier(services)
