package server;

import gen.steam.sales.ProtonDbRank;

import java.util.Set;

public record Game(String title, Set<String> tags, ProtonDbRank protonDbRank, double price) {
}
