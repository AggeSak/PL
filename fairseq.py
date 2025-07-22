import sys

def fair_sequence(filename):
    with open(filename, 'r') as file:
       
        N = int(file.readline().strip())
        S = list(map(int, file.readline().strip().split()))
    
    total_sum = sum(S)
    stoxo = total_sum // 2
    

    dp = [False] * (stoxo + 1)
    dp[0] = True
    
    for num in S:

         
        
        for j in range(stoxo, num - 1, -1):
            if dp[j - num]:
                dp[j] = True
    
    
    for j in range(stoxo, -1, -1):
        if dp[j]:
            closest_sum = j
            break
    
    fair_difference = abs((total_sum - closest_sum) - closest_sum)
    return fair_difference

if __name__ == "__main__":
    if len(sys.argv) != 2:
        #print("wrong")
        sys.exit(1)
    
    filename = sys.argv[1]
    result = fair_sequence(filename)
    print(result)