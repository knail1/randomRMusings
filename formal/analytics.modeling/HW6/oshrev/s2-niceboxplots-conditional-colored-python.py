"""
Scenario:
  An airport has a limited number of servers and scanners
  which take different amount of times to process a passenger

  Passengers arrive at the airport queue at a random time. If one server
   is available, they start the checking process and wait for it
  to finish. If not, they wait until they an use one.  Once a passenger
  has their ID/boading pass checked, they enter the next available scanner.

"""
import random

import simpy

import statistics

import matplotlib.pyplot as plt
import numpy as np


RANDOM_SEED = 42
NUM_SERVERS = 4  # Number of servers in airport queue
NUM_SCANNERS = 4 # Number of scanners in airport queue
CHECKTIME = 0.75  # Average minutes it takes to check a passenger
SCAN_LOW = 0.5 # Minimum time it takes to scan a passenger
SCAN_HIGH = 1.0 # Maximum time it takes to scan a passenger
ARRIVAL_RATE = 5.0 # Average arrivals per minute
SIM_TIME = 120     # Simulation time in minutes
REPLICATIONS = 100 # Number of times to repeat the simulation

class AirportQueue(object):
    """Airport queue has a limited number of servers (``NUM_SERVERS``) to
    check passengers in parallel.

    Passengers have to request one of the servers. When they got one, they
    can start the ID/boarding-pass check processes and wait for it to finish
    (which takes, on average, ``checktime`` minutes).

    """
    def __init__(self, env, num_servers, num_scanners, checktime):
        self.env = env
        self.server = simpy.Resource(env, num_servers)
        self.scanner = [simpy.Resource(env, 1) for i in range(num_scanners)]
        self.checktime = checktime

    def check(self, passenger):
        """The checking processes. It takes a ``passenger`` processes and tries
        to check it."""
        yield self.env.timeout(random.expovariate(1.0 / CHECKTIME))

    def scan(self, passenger):
        """The checking processes. It takes a ``passenger`` processes and tries
        to check it."""
        yield self.env.timeout(random.uniform(SCAN_LOW, SCAN_HIGH))


def passenger(env, name, aq):
    """The passenger process (each passenger has a ``name``) arrives at the airport queue
    (``aq``) and requests a server.

    It then starts the check process, waits for it to finish and
    leaves to never come back ...

    """
    # note arrival time of passenger
    timeArrive = env.now

    # print('%s arrives at the check queue at %.2f.' % (name, timeArrive))


    with aq.server.request() as check_request:
        yield check_request
        timeCheckBegin = env.now

        # print('%s begins the checking process at %.2f.' % (name, env.now))
        yield env.process(aq.check(name))
        timeCheckEnd = env.now

        # print('%s ends the checking process at %.2f.' % (name, env.now))


    # Find the shortest scanner queue
    qlens = [len(aq.scanner[i].queue) for i in range(NUM_SCANNERS)]
    minq = qlens.index(min(qlens))

    with aq.scanner[minq].request() as scan_request:
        yield scan_request
        timeScanBegin = env.now

        # print('%s enters the scanner at %.2f.' % (name, env.now))
        yield env.process(aq.scan(name))
        timeScanEnd = env.now

        # print('%s leaves the scanner at %.2f.' % (name, env.now))

    wait_times.append((timeCheckBegin - timeArrive) + (timeScanEnd - timeCheckEnd))
    #print('Average wait time so far: %.2f.' % (statistics.mean(wait_times)))


def setup(env, num_servers, num_scanners, checktime, arrival_rate):
    """Create an airport queue, a number of initial passengers and keep creating passengers
    at a rate of ``arrival_rate`` every minute."""
    # Create the carwash
    airportq = AirportQueue(env, num_servers, num_scanners, checktime)

    i = 0
    # Create passengers while the simulation is running
    while True:
        yield env.timeout(random.expovariate(arrival_rate))
        i += 1
        env.process(passenger(env, 'Passenger %d' % i, airportq))


# Setup and start the simulation

random.seed(RANDOM_SEED)  # This helps reproducing the results
wait_times_matrix = []

for NUM_SERVERS in range(1, 6):
    wait_times_row = []
    for NUM_SCANNERS in range(1, 6):
        avg_wait_times = []
        wait_times = []
        for i in range(REPLICATIONS):
            # Create an environment and start the setup process
            env = simpy.Environment()
            env.process(setup(env, NUM_SERVERS, NUM_SCANNERS, CHECKTIME, ARRIVAL_RATE))

            # Execute!
            env.run(until=SIM_TIME)
    #        avg_wait_times.append(statistics.mean(wait_times))
        wait_times_row.append(wait_times)
    #    avg_wait_times_row.append(statistics.mean(avg_wait_times))
    wait_times_matrix.append(wait_times_row)
    #avg_wait_times_matrix.append(avg_wait_times_row)


#print(wait_times_matrix[1][1])
#plt.boxplot(wait_times_matrix[0][0])
#plt.show()

# Make a boxplot for each combination of NUM_SERVERS and NUM_SCANNERS

fig, ax = plt.subplots(5, 5, squeeze=True)

fig.set_size_inches(12, 12)

plt.suptitle("Airport wait times", fontsize = 16)

for i in range(5):
    for j in range(5):
        bplot = ax[i, j].boxplot(wait_times_matrix[i][j], medianprops=dict(color="black"))
        ax[i, j].set_xticks([])
        ax[i, j].set_title("(" + str(i + 1) + "," + str(j + 1) + ")")
        med = statistics.median(wait_times_matrix[i][j])
        if (med < 15): color = "darkseagreen"
        else: color = "lightsalmon"
        ax[i, j].set_facecolor(color)
        ax[i, j].text(0.62, med, round(med,1))
        #ax[i, j].annotate('hello', xy = (0,0), xytext=(0,1.5))

fig.savefig('wait_times.png')
