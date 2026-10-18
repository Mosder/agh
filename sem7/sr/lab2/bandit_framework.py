import matplotlib.pyplot as plt
from random import random
from typing import Dict, List, NamedTuple, Optional


class Bandit:
    def __init__(self, bandit_id: str, arm_ids: List[str]):
        self.bandit_id = bandit_id  # name of the bandit to be displayed
        self.arm_ids = arm_ids  # list of IDs of all the existing arms

    def recommend(self, size: int) -> List[str]:
        # must return a list of exactly `size` distinct arm IDs taken from self.arm_ids
        raise NotImplementedError()

    def feedback(self, arm_id: str, reward: int) -> None:
        # called by the Runner after every recommendation, separately for EACH recommended arm;
        # reward is 1 if the arm was clicked, 0 otherwise
        raise NotImplementedError()


class Arm:
    def __init__(self, arm_id: str, click_probability: float):
        self.arm_id = arm_id
        self.click_probability = click_probability  # hidden from the bandits - they have to learn it

    def pull(self) -> int:  # returns 1 (click) with probability click_probability, 0 otherwise
        return 1 if random() < self.click_probability else 0


class InvalidRecommendationException(Exception):
    pass


class SimulationResult(NamedTuple):
    bandit_id: str
    rewards: List[int]  # number of clicks in every epoch
    regrets: List[float]  # regret of every epoch

    @property
    def total_reward(self) -> int:
        return sum(self.rewards)

    @property
    def total_regret(self) -> float:
        return sum(self.regrets)


class Runner:
    def __init__(self, arms: Dict[str, Arm], bandits: List[Bandit]):
        bandit_ids = [bandit.bandit_id for bandit in bandits]
        if len(bandit_ids) != len(set(bandit_ids)):
            raise ValueError(f'Bandit IDs must be unique: {bandit_ids}')
        self.arms = arms
        self.bandits = bandits
        self.recommendation_size: Optional[int] = None

    # runs every bandit for `epochs` epochs; in each epoch the bandit recommends `recommendation_size` arms,
    #   every recommended arm is pulled and its reward is passed back to the bandit through feedback()
    # returns the reward and the regret of every epoch, for every bandit
    def simulate(self, epochs: int, recommendation_size: int) -> List[SimulationResult]:
        self.recommendation_size = recommendation_size
        best_expected_reward = self.best_expected_reward(recommendation_size)
        results = []
        for bandit in self.bandits:
            print(f'Simulating: {bandit.bandit_id}')
            rewards, regrets = [], []
            for epoch in range(epochs):
                recommendation = bandit.recommend(recommendation_size)
                self._validate(recommendation, recommendation_size)

                epoch_reward = 0
                for arm_id in recommendation:
                    reward = self.arms[arm_id].pull()
                    epoch_reward += reward
                    bandit.feedback(arm_id, reward)

                expected_reward = sum(self.arms[arm_id].click_probability for arm_id in recommendation)
                rewards.append(epoch_reward)
                regrets.append(best_expected_reward - expected_reward)
            results.append(SimulationResult(bandit.bandit_id, rewards, regrets))
        return results

    def best_expected_reward(self, recommendation_size: int) -> float:
        probabilities = sorted((arm.click_probability for arm in self.arms.values()), reverse=True)
        return sum(probabilities[:recommendation_size])

    def _validate(self, recommendation: List[str], size: int) -> None:
        if len(recommendation) != size:
            raise InvalidRecommendationException(
                f'Recommendation should contain {size} arms, got {len(recommendation)}: {recommendation}')
        if len(set(recommendation)) != len(recommendation):
            raise InvalidRecommendationException(f'Recommendation {recommendation} contains duplicated entries!')
        unknown = [arm_id for arm_id in recommendation if arm_id not in self.arms]
        if unknown:
            raise InvalidRecommendationException(f'Recommendation contains unknown arms: {unknown}')

    # mode='regret' - cumulative regret (lower is better)
    # mode='reward' - reward per epoch, smoothed with a moving average over `window` epochs (higher is better)
    def plot_results(self, results: List[SimulationResult], mode: str = 'regret', scale: str = 'linear',
                     window: int = 20) -> None:
        plt.figure(figsize=(10, 5))
        for result in results:
            if mode == 'regret':
                plt.plot(self._cumulative(result.regrets), label=result.bandit_id)
            elif mode == 'reward':
                plt.plot(self._moving_average(result.rewards, window), label=result.bandit_id)
            else:
                raise ValueError(f"Unknown mode: {mode}, use 'regret' or 'reward'")

        if mode == 'regret':
            plt.ylabel('cumulative regret')
        else:
            plt.axhline(self.best_expected_reward(self.recommendation_size), color='gray', linestyle='--',
                        label='best expected reward')
            plt.ylabel(f'reward per epoch (moving average, window={window})')
        plt.xlabel('epoch')
        plt.yscale(scale)
        plt.legend(loc='upper left', bbox_to_anchor=(1, 1))
        plt.tight_layout()
        plt.show()

    def print_summary(self, results: List[SimulationResult]) -> None:
        print(f"\n{'bandit':<20}{'total reward':>15}{'total regret':>15}")
        for result in sorted(results, key=lambda result: result.total_regret):
            print(f'{result.bandit_id:<20}{result.total_reward:>15}{result.total_regret:>15.1f}')

    @staticmethod
    def _cumulative(values: List[float]) -> List[float]:
        cumulative, total = [], 0.0
        for value in values:
            total += value
            cumulative.append(total)
        return cumulative

    @staticmethod
    def _moving_average(values: List[float], window: int) -> List[float]:
        return [sum(values[max(0, i - window + 1):i + 1]) / (i + 1 - max(0, i - window + 1))
                for i in range(len(values))]
