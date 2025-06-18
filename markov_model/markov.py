import csv
import numpy as np
import os
import pandas as pd
import scipy.linalg as sp
import random

def calculate_woba(homeruns, triples, doubles, singles, walks, outs, at_bats):
    wBB, w1B, w2B, w3B, wHR = 0.55, 0.7, 1, 1.27, 1.65  # MLB 2015 weights
    return (wBB * walks + w1B * singles + w2B * doubles + w3B * triples + wHR * homeruns) / at_bats

def player_matrix(homeruns, triples, doubles, singles, walks, outs, at_bats, player_name="Unknown"):
    # Calculate plate appearances
    pa = homeruns + triples + doubles + singles + walks + outs
    
    # Debug: Print raw outcomes
    print(f"Player: {player_name}, PA={pa}, AB={at_bats}, Outcomes sum={homeruns + triples + doubles + singles + outs}")
    
    # Smooth stats for low PAs
    min_pa = 10
    league_avg = {'h': 0.04, 't': 0.005, 'd': 0.05, 's': 0.15, 'w': 0.08, 'o': 0.68}
    if pa < min_pa:
        weight = pa / min_pa
        league_pa = min_pa - pa
        homeruns = homeruns * weight + league_avg['h'] * league_pa
        triples = triples * weight + league_avg['t'] * league_pa
        doubles = doubles * weight + league_avg['d'] * league_pa
        singles = singles * weight + league_avg['s'] * league_pa
        walks = walks * weight + league_avg['w'] * league_pa
        outs = outs * weight + league_avg['o'] * league_pa
        pa = min_pa

    h = homeruns / pa
    t = triples / pa
    d = doubles / pa
    s = singles / pa
    w = walks / pa
    o = outs / pa

    # Verify probabilities sum to 1
    total_prob = h + t + d + s + w + o
    if abs(total_prob - 1.0) > 1e-3:  # Relaxed tolerance
        print(f"Warning: Probabilities sum to {total_prob} for {player_name}, normalizing...")
        h /= total_prob
        t /= total_prob
        d /= total_prob
        s /= total_prob
        w /= total_prob
        o /= total_prob


    B = np.array([
        [h, w+s,   d, t,       0,   0,   0,       0],
        [h,   0, d/2, t, w+(s/2), s/2, d/2,       0],
        [h, s/2,   d, t,       w, s/2,   0,       0],
        [h,   s,   d, t,       0,   w,   0,       0],
        [h,   0, d/2, t,     s/6, s/3, d/2, w+(s/2)],
        [h,   0, d/2, t,     s/2, s/2, d/2,       w],
        [h, s/2,   d, t,       0, s/2,   0,       w],
        [h,   0, d/2, t,     s/2, s/2, d/2,       w],
    ])

    I = np.zeros([8,8])
    np.fill_diagonal(I, o)
    V = np.full([8,1], o)
    tb = sp.block_diag(B, B, B, B, B, B, B, B, B, B, B, B, B, B, B, B, B, B, B, B, B, B, B, B, B, B, B, 0)
    v8 = np.block([V, np.zeros([8,7])])
    one = np.ones((1,1))
    v9 = np.concatenate((V, one), axis=0)
    offset = 8
    aux = np.empty((0, offset), int)
    ti = sp.block_diag(aux, I, I, v8, I, I, v8, I, I, v8, I, I, v8, I, I, v8, I, I, v8, I, I, v8, I, I, v8, I, I, v9)
    T = tb + ti
    return T

def game_matrix(file_name_input):
    f = open(file_name_input)
    csv_f = csv.DictReader(f)
    game_T_matrix = []
    players = []

    for row in csv_f:
        homeruns = float(row['homeruns'])
        triples = float(row['triples'])
        doubles = float(row['doubles'])
        singles = float(row['singles'])
        walks = float(row['walks'])
        outs = float(row['outs'])
        at_bats = float(row['at_bats'])
        player_name = row['player_name']
        woba = calculate_woba(homeruns, triples, doubles, singles, walks, outs, at_bats)
        players.append({
            'name': player_name,
            'order': int(row['order']),
            'woba': woba,
            'ab': at_bats,
            'stats': [homeruns, triples, doubles, singles, walks, outs, at_bats],
            'pos': row['position']
        })
        player_T_matrix = player_matrix(homeruns, triples, doubles, singles, walks, outs, at_bats, player_name)
        game_T_matrix.append(player_T_matrix)
    
    f.close()
    return game_T_matrix, players

def run_value_matrix():
    N = np.array([
        [1, 0, 0, 0, 0, 0, 0, 0],
        [1.5, 0.7, 0.7, 0.7, 0, 0, 0, 0],
        [1.5, 0.7, 0.7, 0.7, 0, 0, 0, 0],
        [1.5, 0.7, 0.7, 0.7, 0, 0, 0, 0],
        [2.5, 1.5, 1.5, 1.5, 0.7, 0.7, 0.7, 0],
        [2.5, 1.5, 1.5, 1.5, 0.7, 0.7, 0.7, 0],
        [2.5, 1.5, 1.5, 1.5, 0.7, 0.7, 0.7, 0],
        [3.5, 2.5, 2.5, 2.5, 1.5, 1.5, 1.5, 0.7]
    ])
    R = sp.block_diag(N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, 0)
    return R

def current_state():
    C = np.zeros([1,217])
    C[0,0] = 1
    return C

def play_ball(lineup_indices, C, R, game_T_matrix, max_iterations=27):
    total_runs = 0
    hitter = 1
    iterations = 0
    while C[0,216] < 0.95 and iterations < max_iterations:
        i = lineup_indices[hitter - 1]
        T = game_T_matrix[i]
        runs = np.dot(C, R*T)
        total_runs += np.sum(runs)
        C = np.dot(C, T)
        hitter = hitter + 1
        if hitter > 9:
            hitter = 1
        iterations += 1
    return total_runs

def clean_csv(file_name_input):
    if not os.path.exists(file_name_input):
        print("ERROR: Unable to find", file_name_input, "in current working directory.")
        files = os.listdir(os.getcwd())
        csv_files = [i for i in files if i.endswith('.csv')]
        for i in csv_files:
            print(i)
        file_name_input = input("Please enter one of the .csv file names listed above:\n")
        return file_name_input
    return file_name_input

def lineup_card(file_name_input, lineup_indices, players):
    df = pd.read_csv(file_name_input, index_col=0)
    lineup_card = []
    for index, i in enumerate(lineup_indices, start=1):
        player = players[i]
        lineup_card.append((index, player['order'], player['name'], player['pos']))
    
    lineup_card_df = pd.DataFrame(lineup_card, columns=['#', 'order', 'player_name', 'position'])
    print(lineup_card_df.to_string(index=False))
    return lineup_card_df

def find_best_lineup(players, game_T_matrix, max_players=30):
    if len(players) < 9 or len(players) > max_players:
        raise ValueError(f"Number of players must be between 9 and {max_players}.")
    
    C = current_state()
    R = run_value_matrix()
    
    # Filter players with at least 5 AB
    players = [p for p in players if p['ab'] >= 5]
    if len(players) < 9:
        raise ValueError(f"Not enough players with 5+ at-bats. Found {len(players)} players.")
    
    # Check for at least one catcher
    catchers = [i for i, p in enumerate(players) if p['pos'] == 'c']
    if not catchers:
        raise ValueError("No players with position 'c' (catcher) available.")
    
    # Initial lineup: top 8 by wOBA + 1 catcher (highest wOBA among catchers)
    sorted_players = sorted(enumerate(players), key=lambda x: x[1]['woba'], reverse=True)
    # Select the highest-wOBA catcher
    catcher_indices = [i for i, p in sorted_players if players[i]['pos'] == 'c']
    initial_catcher_idx = catcher_indices[0]  # Highest wOBA catcher
    
    # Select top players, ensuring the catcher is included
    current_indices = []
    for i, _ in sorted_players:
        if len(current_indices) < 9 and i not in current_indices:
            current_indices.append(i)
    
    if len(current_indices) != 9:
        raise ValueError(f"Failed to construct a lineup with 9 players. Got {len(current_indices)} players.")
    
    current_xrv = play_ball(current_indices, C.copy(), R, game_T_matrix)
    
    # Simulated annealing
    temp = 1000
    cooling_rate = 0.95
    iterations = 1000
    
    best_indices = current_indices.copy()
    best_xrv = current_xrv
    
    for _ in range(iterations):
        temp *= cooling_rate
        # Generate neighbor: swap two players or replace one with a bench player
        if random.random() < 0.5 and len(players) > 9:
            # Replace one lineup player with a bench player
            lineup_idx = random.randint(0, 8)
            bench_indices = [i for i, _ in enumerate(players) if i not in current_indices]
            if bench_indices:
                new_idx = random.choice(bench_indices)
                neighbor = current_indices.copy()
                neighbor[lineup_idx] = new_idx
                # Ensure neighbor has at least one catcher
                if not any(players[i]['pos'] == 'c' for i in neighbor):
                    neighbor[lineup_idx] = random.choice(catcher_indices)
        else:
            # Swap two players in lineup
            neighbor = current_indices.copy()
            i, j = random.sample(range(9), 2)
            neighbor[i], neighbor[j] = neighbor[j], neighbor[i]
            # Ensure neighbor has at least one catcher
            if not any(players[i]['pos'] == 'c' for i in neighbor):
                continue
        
        neighbor_xrv = play_ball(neighbor, C.copy(), R, game_T_matrix)
        
        # Accept neighbor if better or with probability based on temp
        if neighbor_xrv > current_xrv or random.random() < np.exp((neighbor_xrv - current_xrv) / temp):
            current_indices = neighbor
            current_xrv = neighbor_xrv
        
        if current_xrv > best_xrv:
            best_indices = current_indices.copy()
            best_xrv = current_xrv
    
    return best_indices, best_xrv

def main():
    file_name_input = 'markov_model/stats_vs_right.csv'
    file_name_input = clean_csv(file_name_input)
    
    game_T_matrix, players = game_matrix(file_name_input)
    
    print("\nFinding optimal lineup...")
    best_lineup_indices, expected_runs = find_best_lineup(players, game_T_matrix)
    
    print("\nThe following batting order...")
    df = lineup_card(file_name_input, best_lineup_indices, players)
    df.to_csv('groundsloths.csv',index=False)
    print(f"\nwill produce {expected_runs:.3f} expected runs per game!\n")

if __name__ == "__main__":
    main()