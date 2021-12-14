def part_one(actions: [(str, int)]):
    horizontal_pos = 0
    depth = 0
    for (action, amount) in actions:
        if action == 'forward':
            horizontal_pos = horizontal_pos + amount
        elif action == 'down':
            depth = depth + amount
        elif action == 'up':
            depth = depth - amount
    print(f"""Part one. Horizontal pos: {horizontal_pos},
              depth: {depth}, multiplied {horizontal_pos * depth}""")


def part_two(actions: [(str, int)]):
    horizontal_pos = 0
    depth = 0
    aim = 0
    for (action, amount) in actions:
        if action == 'forward':
            horizontal_pos = horizontal_pos + amount
            depth = depth + (aim * amount)
        elif action == 'down':
            aim = aim + amount
        elif action == 'up':
            aim = aim - amount
    print(f"""Part two. Horizontal pos: {horizontal_pos}, depth: {depth},
              aim: {aim}, multiplied {horizontal_pos * depth}""")


def main():
    with open('exercise2_input.txt', 'r') as input_file:
        # First split in newines, then split each line on
        # space and convert amount to int
        actions = [(x.split(' ')[0], int(x.split(' ')[1]))
                   for x in input_file.read().split("\n") if x != '']
    part_one(actions)
    part_two(actions)


if __name__ == '__main__':
    main()
