import pandas as pd

# Define base path
base_path = 'data/DSL-Keystroke-KSK/data-typing/'
files = ['simple.txt', 'strong.txt', 'numeric.txt']

df_list = []

for f in files:
    path = base_path + f
    print(f"🔍 Reading {path}...")

    # Try to detect the right delimiter automatically
    with open(path, 'r', encoding='utf-8') as file:
        first_line = file.readline()
        # Guess the delimiter (tab or multiple spaces)
        if '\t' in first_line:
            sep = '\t'
        else:
            sep = r'\s+'

    # Read file with correct delimiter
    temp_df = pd.read_csv(path, sep=sep, engine='python')

    # Add source column
    temp_df['source'] = f.replace('.txt', '')
    df_list.append(temp_df)

# Combine all dataframes
combined_df = pd.concat(df_list, ignore_index=True)

# Save cleaned dataset
combined_df.to_csv('data/keystroke_combined.csv', index=False)

print("\n✅ Cleaned dataset saved as 'data/keystroke_combined.csv'")
print("Shape:", combined_df.shape)
print("Sample columns:\n", list(combined_df.columns)[:10])

# -----------------------------------------------------------------------
# Step 1: Data Cleaning
# -----------------------------------------------------------------------

#Drop duplicates
combined_df = combined_df.drop_duplicates()

#Drop rows only if *all* values are NaN
combined_df = combined_df.dropna(how='all')

# Fill remaining missing numeric values with column mean 
combined_df = combined_df.fillna(combined_df.mean(numeric_only=True))

print("Cleaned missing values and duplicates")
print("Shape after cleaning:", combined_df.shape)

# -----------------------------------------------------------------------
# Step 2: Feature Normalization
# -----------------------------------------------------------------------

from sklearn.preprocessing import StandardScaler

# Select only numeric columns for scaling 

numeric_cols = combined_df.select_dtypes(include=['float64', 'int64']).columns

scaler = StandardScaler()
combined_df[numeric_cols] = scaler.fit_transform(combined_df[numeric_cols])

print("Normalized numeric features")

# Save cleaned and normalized dataset
combined_df.to_csv('data/keystroke_cleaned_scaled.csv', index = False)
print("Saved 'keystroke_cleaned_scaled.csv'")
