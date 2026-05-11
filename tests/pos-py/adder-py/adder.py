class Adder:
    def __init__(self, seed):
        self.seed = seed

    def add(self, a, b):
        return a * self.seed + b
