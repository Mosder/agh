package server;

import com.google.protobuf.ProtocolStringList;
import gen.steam.sales.SaleNotification;
import io.grpc.stub.ServerCallStreamObserver;

import java.util.ArrayDeque;
import java.util.Deque;
import java.util.Set;

public class ClientSession {
    private final static int MAX_BUFFER_SIZE = 20;
    private final Deque<SaleNotification> buffer;
    private ServerCallStreamObserver<SaleNotification> observer;
    private Set<String> tags;

    public ClientSession(Set<String> tags) {
        this.tags = tags;
        this.buffer = new ArrayDeque<>();
        this.observer = null;
    }

    synchronized public void setTags(Set<String> tags) {
        this.tags = tags;
    }

    synchronized public void addTags(Set<String> tags) {
        this.tags.addAll(tags);
    }

    synchronized public void removeTags(Set<String> tags) {
        this.tags.removeAll(tags);
    }

    synchronized public void setObserver(ServerCallStreamObserver<SaleNotification> observer) {
        this.observer = observer;
    }

    synchronized public boolean matchInTags(ProtocolStringList tags) {
        for (String tag : tags) {
            if (this.tags.contains(tag)) return true;
        }
        return false;
    }

    synchronized public void notify(SaleNotification notif) {
        if (observer != null && !observer.isCancelled()) {
            try {
                observer.onNext(notif);
                return;
            } catch (Exception e) {
                observer = null;
            }
        }
        buffer.add(notif);
        while (buffer.size() > MAX_BUFFER_SIZE) {
            buffer.removeFirst();
        }
    }

    synchronized public void sendBuffered() {
        while (observer != null && !observer.isCancelled() && !buffer.isEmpty()) {
            try {
                observer.onNext(buffer.removeFirst());
            } catch (Exception e) {
                observer = null;
            }
        }
    }
}
