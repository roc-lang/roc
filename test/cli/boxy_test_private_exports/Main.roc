import Relay

seed = Relay.run(4)
expect seed == 5
expect Relay.run(6) == 7
