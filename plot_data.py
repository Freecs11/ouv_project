import pandas as pd
import matplotlib.pyplot as plt

# Read the experimental data from the CSV file
data = pd.read_csv("experimental_data.csv")

# Extract the columns from the data
size = data['size']
gen_time = data['gen_time']
gen_memory = data['gen_memory']
comp_time = data['comp_time']
comp_memory = data['comp_memory']
compression_rate = data['compression_rate']

# Create separate subplots for each graph
fig, axs = plt.subplots(3, 2, figsize=(12, 16))
fig.suptitle('Experimental Data')

# Graph 1: Time it takes to generate a non-compressed graph
axs[0, 0].plot(size, gen_time, marker='o', linestyle='-')
axs[0, 0].set_title('Time to Generate Non-Compressed Graph')
axs[0, 0].set_xlabel('Size')
axs[0, 0].set_ylabel('Time (seconds)')

# Graph 2: Memory space it takes to generate a non-compressed graph
axs[0, 1].plot(size, gen_memory, marker='o', linestyle='-')
axs[0, 1].set_title('Memory Space to Generate Non-Compressed Graph')
axs[0, 1].set_xlabel('Size')
axs[0, 1].set_ylabel('Memory (bytes)')

# Graph 3: Time it takes to compress a graph
axs[1, 0].plot(size, comp_time, marker='o', linestyle='-')
axs[1, 0].set_title('Time to Compress a Graph')
axs[1, 0].set_xlabel('Size')
axs[1, 0].set_ylabel('Time (seconds)')

# Graph 4: Memory space it takes to compress a graph
axs[1, 1].plot(size, comp_memory, marker='o', linestyle='-')
axs[1, 1].set_title('Memory Space to Compress a Graph')
axs[1, 1].set_xlabel('Size')
axs[1, 1].set_ylabel('Memory (bytes)')

# Graph 5: Compression rate
axs[2, 0].plot(size, compression_rate, marker='o', linestyle='-')
axs[2, 0].set_title('Compression Rate')
axs[2, 0].set_xlabel('Size')
axs[2, 0].set_ylabel('Compression Rate')

# Remove the empty subplot
fig.delaxes(axs[2, 1])

# Adjust the layout for better spacing
plt.tight_layout(rect=[0, 0, 1, 0.95])

# Show the graphs
plt.show()
