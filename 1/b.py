DIGITS = {
    "one": "1",
    "two": "2",
    "three": "3",
    "four": "4",
    "five": "5",
    "six": "6",
    "seven": "7",
    "eight": "8",
    "nine": "9",
}

def word_to_num(word: str) -> int:
    nums = []
    for i, ch in enumerate(word):
        # Numeric character
        if ch in DIGITS.values():
            nums.append(ch)
            continue
        # Check if previous length 3, 4, 5 strings are number strings e.g. 'three' 'one'
        a = word[i-4:i+1]
        b = word[i-3:i+1]
        c = word[i-2:i+1]

        for x in [a,b,c]:
            if x in DIGITS:
                nums.append(DIGITS[x])

    return int(nums[0]) * 10 + int(nums[-1])


# Regex version: replace the string numbers with digit chars
# e.g. 'three' -> '3' and use part 1 solution after

def read_words(input_file):
    with open(input_file, 'r') as f:
        return f.readlines()

def main(input_file):
    return sum(map(word_to_num, read_words(input_file)))

if __name__ == "__main__":
    input_file =  "/home/ajpkim/pg/advent-code-2023/1/input.txt"
    res = main(input_file)
    print(res)
