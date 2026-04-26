package server;

import gen.steam.sales.*;
import io.grpc.stub.ServerCallStreamObserver;
import io.grpc.stub.StreamObserver;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

public class SalesService extends SalesSubscriptionServiceGrpc.SalesSubscriptionServiceImplBase {
    private final ConcurrentHashMap<String, ClientSession> clients;
    private final ScheduledExecutorService scheduler;
    private final List<Game> games = List.of(
            new Game("PAYDAY 2", Set.of("shooter", "action", "stealth"), ProtonDbRank.PLATINUM, 29.99),
            new Game("Terraria", Set.of("sandbox", "open-world"), ProtonDbRank.NATIVE, 45.99),
            new Game("Celeste", Set.of("platformer"), ProtonDbRank.NATIVE, 59.99),
            new Game("Slay the Spire 2", Set.of("turn-based", "cards"), ProtonDbRank.NATIVE, 74.99),
            new Game("Silksong", Set.of("metroidvania", "action"), ProtonDbRank.NATIVE, 74.99),
            new Game("ULTRAKILL", Set.of("shooter", "action"), ProtonDbRank.PLATINUM, 99.99),
            new Game("XCOM 2", Set.of("turn-based", "strategy"), ProtonDbRank.GOLD, 89.00)
    );

    public SalesService() {
        clients = new ConcurrentHashMap<>();
        scheduler = Executors.newSingleThreadScheduledExecutor();
    }

    private SaleNotification generateNotification() {
        Random random = new Random();
        int randomIndex = random.nextInt(games.size());
        Game game = games.get(randomIndex);
        int discount = random.nextInt(10, 91);
        double salePrice = game.price() * (100 - discount) / 100;

        SaleInfo saleInfo = SaleInfo.newBuilder()
                .setOriginalPrice(game.price())
                .setDiscountPercent(discount)
                .setSalePrice(salePrice)
                .build();

        return SaleNotification.newBuilder()
                .setGameTitle(game.title())
                .addAllGameTags(game.tags())
                .setSaleInformation(saleInfo)
                .setProtondbRank(game.protonDbRank())
                .build();
    }

    public void startGenerator() {
        scheduler.scheduleAtFixedRate(() -> {
            SaleNotification notif = generateNotification();
            clients.forEach((id, session) -> {
                if (session.matchInTags(notif.getGameTagsList())) {
                    session.notify(notif);
                }
            });
        }, 1, 3, TimeUnit.SECONDS);
    }

    public void shutdown() {
        scheduler.shutdown();
    }

    @Override
    public void subscribe(RequestData request, StreamObserver<SaleNotification> response) {
        String id = request.getClientId();
        if (id.isBlank()) {
            id = "anonymous";
        }
        Set<String> tags = new HashSet<>(request.getTagsList());

        ClientSession session;
        if (clients.containsKey(id)) {
            session = clients.get(id);
            session.setTags(tags);
        }
        else {
            session = new ClientSession(tags);
            clients.put(id, session);
        }

        ServerCallStreamObserver<SaleNotification> observer = (ServerCallStreamObserver<SaleNotification>) response;
        observer.setOnCancelHandler(() -> session.setObserver(null));
        session.setObserver(observer);
        session.sendBuffered();
    }

    @Override
    public void unsubscribe(UnsubscribeRequestData request, StreamObserver<com.google.protobuf.Empty> response) {
        String id = request.getClientId();
        clients.remove(id);
        response.onNext(com.google.protobuf.Empty.getDefaultInstance());
        response.onCompleted();
    }

    @Override
    public void addTags(RequestData request, StreamObserver<com.google.protobuf.Empty> response) {
        String id = request.getClientId();
        if (clients.containsKey(id)) clients.get(id).addTags(new HashSet<>(request.getTagsList()));
        response.onNext(com.google.protobuf.Empty.getDefaultInstance());
        response.onCompleted();
    }

    @Override
    public void removeTags(RequestData request, StreamObserver<com.google.protobuf.Empty> response) {
        String id = request.getClientId();
        if (clients.containsKey(id)) clients.get(id).removeTags(new HashSet<>(request.getTagsList()));
        response.onNext(com.google.protobuf.Empty.getDefaultInstance());
        response.onCompleted();
    }
}
