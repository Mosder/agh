import socket;

serverIP = "127.0.0.1"
serverPort = 9011
msg = "Python"

print('PYTHON UDP CLIENT')
client = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
client.sendto(bytes(msg, 'utf-8'), (serverIP, serverPort))

buff, _ = client.recvfrom(1024)
print(str(buff))

