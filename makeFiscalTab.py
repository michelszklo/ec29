import pandas as pd
import re


def format_numbers(text):
    # Function to format a matched number
    def format_match(match):
        number = match.group(0)
        # Check if the number is enclosed in parentheses
        if number.startswith('(') and number.endswith(')'):
            number_inside = float(number[1:-1])  # Convert number inside parentheses
            return f"({number_inside:.3f})"
        else:
            # Check if the number has trailing symbols like '**'
            match_result = re.match(r"(-?\d*\.?\d+)(.*)", number)
            if match_result:
                number_value = float(match_result.group(1))
                trailing = match_result.group(2)
                return f"{number_value:.3f}{trailing}"
            return f"{float(number):.3f}"

    # Regular expression to find numbers (including those in parentheses)
    formatted_text = re.sub(r"(\(?-?\d*\.?\d+\)?(\*\*|\*|))", format_match, text)
    return formatted_text

# Load the Excel file
file_path = '/home/dcc213/investigacion/2021/decentralization/github/ec29/regs_outputs/regression_tables_raw_28Aug2024.xlsx'
sheet_name = 'fiscal_response'


# Read the specific sheet
df = pd.read_excel(file_path, sheet_name=sheet_name)

# Extract the variable names (assuming they are in the first column)
variables = df.iloc[::2, 0].reset_index(drop=True)  # Extract variable names from every other row starting at 0

# Extract coefficients (from odd rows) and standard errors (from even rows)
coefficients = df.iloc[::2, 0:].reset_index(drop=True)
std_errors = df.iloc[1::2, 0:].reset_index(drop=True)

print(coefficients)
print(std_errors)


terms = ["Total Revenue per capita (log)", "Total Spending per capita (log)", "Health and Sanitation Spending per capita (log)", "Non-Health Spending per capita (log)", "Non-Health Social Spending per capita (log)", "Non-Social Spending per capita (log)", "Health Spending per capita - Total (log)", "Health Spending per capita - Own Resources (log)", "Health Spending per capita - Other Resources (log)", "Health Spending per capita - Personnel (log)", "Health Spending per capita - Investment (log)", "Health Spending per capita - Outsourced (3rd parties services) (log)", "Health Spending per capita - Admin, Management, others (log)"]
names = ["Total Revenues", "Total Spending", "&&&&\\\\ Health Spending", "Non-Health Spending", "Non-Health Social Spending", "Non-Social Spending", "&&&&\\\\ \\midrule \\textbf{Panel B: SIOPS}&&&&\\\\ Total Health Spending", "&&&&\\\\ From Own Resources", "From Other Resources", "&&&&\\\\ Personnel", "Investment", "Outsourced (3\\textsuperscript{rd} party services)", "Admin, Management and Others"]

with open('fiscalSpend_all_noOutlier.tex', 'w') as file:
    file.write("\\begin{table}[htpb!] \n \\centering \n \\caption{Fiscal Reactions, in natural logarithm of Reais per capita} \n")
    file.write("\\scalebox{0.92}{\\begin{tabular}{lcccc} \\toprule \n & \\multicolumn{4}{c}{ln(Spending)}\\\\ \cmidrule(r){2-5}\n")
    file.write("& (1) & (2) & (3) & (4) \\\\ \\midrule \n")
    file.write("\\textbf{Panel A: FINBRA}&&&&\\\\\n")
    for pos, select_term in enumerate(terms):
        t1 = names[pos]

        # Collect betas and standard errors for each specification
        betas = []
        ses = []
    
        for spec in range(1, 5):
            beta = coefficients.loc[(coefficients['term'] == select_term) & (coefficients['spec'] == spec)].iloc[0, 1]
            se = std_errors.loc[(std_errors['term'] == select_term) & (std_errors['spec'] == spec)].iloc[0, 1]
            beta=format_numbers(beta)
            se = format_numbers(se)
            
            betas.append(beta)
            ses.append(f"{se}")
        
        # Print the formatted output
        file.write(f"{t1} & " + " & ".join(betas) + " \\\\\n")
        file.write(" & " + " & ".join(ses) + " \\\\\n")
    file.write("\\midrule \n Mun FE, Time-State FE, Data Quality Control & Y&Y&Y&Y\\\\ \n ")
    file.write("Baseline Socioeconomic Controls $\\times$ Time & N&Y&Y&Y\\\\ \n ")
    file.write("Time-Varying Controls & N&N&Y&Y\\\\ \n ")
    file.write("Fiscal Controls & N&N&N&Y\\\\ \\bottomrule \n ")
    file.write("\\multicolumn{5}{p{16cm}}{{\\footnotesize \\textbf{Notes:} Each cell represents a separate regression of spending or revenue on exposure to the EC/29 reform, following \\eqref{eq:1}. The number of observations is 63,590 for FINBRA variables and 55,469 for SIOPS variables.  Column 1 presents the baseline model with municipality and state-year fixed effects, plus data quality controls. Column 2 adds baseline socioeconomic controls from the Census interacted with time. Column 3 adds controls for GDP per capita and \emph{Bolsa Familia} transfers per capita. Column 4 adds fiscal controls; namely neighbouring municipality spending and exposure to the LRF. Covariates are omitted for ease of presentation. Standard errors presented in parentheses are clustered at the municipality level. $^{*}$ p$<$0.10; $^{**}$ p $<$ 0.05; $^{***}$ p $<$0.01.}}\n")
    file.write("\\end{tabular}} \n \\end{table} \n")


    
##############BALANCE
terms = ["Total Revenue per capita (log)", "Total Spending per capita (log)", "Health and Sanitation Spending per capita (log)", "Non-Health Spending per capita (log)", "Non-Health Social Spending per capita (log)", "Non-Social Spending per capita (log)", "Health Spending per capita - Total (log)", "Health Spending per capita - Own Resources (log)", "Health Spending per capita - Other Resources (log)", "Health Spending per capita - Personnel (log)", "Health Spending per capita - Investment (log)", "Health Spending per capita - Outsourced (3rd parties services) (log)", "Health Spending per capita - Admin, Management, others (log)"]
names = ["Total Revenues", "Total Spending", "&&&&\\\\ Health Spending", "Non-Health Spending", "Non-Health Social Spending", "Non-Social Spending", " \\midrule \\textbf{Panel B: SIOPS}&&&&\\\\ Total Health Spending", "&&&&\\\\ From Own Resources", "From Other Resources", "&&&&\\\\ Personnel", "Investment", "Outsourced (3\\textsuperscript{rd} party services)", "Admin, Management and Others"]
sheets=['finbra','finbra','finbra','finbra','finbra','finbra','siops','siops','siops','siops','siops','siops','siops']

def replace_stars(match):
    """Function to replace stars with LaTeX superscript."""
    stars = match.group(0)
    if stars == '***':
        return '$^{***}$'
    elif stars == '**':
        return '$^{**}$'
    elif stars == '*':
        return '$^{*}$'


with open('fiscalSpend_balance_noOutlier.tex', 'w') as file:

    file.write("\\begin{table}[htpb!] \n \\centering \n \\caption{Fiscal Reactions, in natural logarithm of Reais per capita} \\label{table:fiscal} \n")
    file.write("\\scalebox{0.92}{\\begin{tabular}{lcccc} \\toprule \n & \\multicolumn{4}{c}{ln(Spending)}\\\\ \cmidrule(r){2-5}\n")
    file.write("& \\ \\ \\ \\ \\ \\  (1) \\ \\ \\ \\ \\ \\ ")
    file.write("& \\ \\ \\ \\ \\ \\  (2) \\ \\ \\ \\ \\ \\ ")
    file.write("& \\ \\ \\ \\ \\ \\  (3) \\ \\ \\ \\ \\ \\ ")
    file.write("& \\ \\ \\ \\ \\ \\  (4) \\ \\ \\ \\ \\ \\  \\\\ \\midrule \n")
    file.write("\\textbf{Panel A: FINBRA}&&&&\\\\\n")
    for pos, select_term in enumerate(terms):
        t1 = names[pos]

        # Load the Excel file
        sheet_name = 'fiscal_response_'+sheets[pos]
        # Read the specific sheet
        df = pd.read_excel(file_path, sheet_name=sheet_name)
        # Extract coefficients (from odd rows) and standard errors (from even rows)
        coefficients = df.iloc[::2, 0:].reset_index(drop=True)
        std_errors = df.iloc[1::2, 0:].reset_index(drop=True)


        
        # Collect betas and standard errors for each specification
        betas = []
        ses = []
        Ns  = []
        print(pos)
        for spec in range(1, 5):
            beta = coefficients.loc[(coefficients['term'] == select_term) & (coefficients['spec'] == spec)].iloc[0, 1]
            beta = re.sub(r'\*\*\*|\*\*|\*', replace_stars, beta)
            se = std_errors.loc[(std_errors['term'] == select_term) & (std_errors['spec'] == spec)].iloc[0, 1]
            N = coefficients.loc[(coefficients['term'] == select_term) & (coefficients['spec'] == spec)].iloc[0, 2]
            beta=format_numbers(beta)
            se = format_numbers(se)
            betas.append(beta)
            Ns.append(f"{N}")
            ses.append(f"{se}")
            
        # Print the formatted output
        file.write(f"{t1} & " + " & ".join(betas) + " \\\\\n")
        file.write(" & " + " & ".join(ses) + " \\\\\n")
        if pos==5 or pos==12:
            print("adding obs")
            print(Ns)
            file.write("&&&&\\\\ Observations (Each cell) &"+"&".join(Ns) + " \\\\\n")
    file.write("\\midrule \n Mun FE, Time-State FE, Data Quality Control & Y&Y&Y&Y\\\\ \n ")
    file.write("Baseline Socioeconomic Controls $\\times$ Time & N&Y&Y&Y\\\\ \n ")
    file.write("Time-Varying Controls & N&N&Y&Y\\\\ \n ")
    file.write("Fiscal Controls & N&N&N&Y\\\\ \\bottomrule \n ")
    file.write("\\multicolumn{5}{p{17cm}}{{\\footnotesize \\textbf{Notes:} Each cell represents a separate regression of spending or revenue on exposure to the EC/29 reform, following \\eqref{eq:1}. The number of observations in each cell is indicated at the foot of each panel (all spending outcomes are observed for each observation) within FINBRA (panel A) and SIOPS (panel B) measures.  Column 1 presents the baseline model with municipality and state-year fixed effects, plus data quality controls. Column 2 adds baseline socioeconomic controls from the Census interacted with time. Column 3 adds controls for GDP per capita and \emph{Bolsa Familia} transfers per capita. Column 4 adds fiscal controls; namely neighbouring municipality spending and exposure to the LRF. Covariates are omitted for ease of presentation. Standard errors presented in parentheses are clustered at the municipality level. $^{*}$ p$<$0.10; $^{**}$ p $<$ 0.05; $^{***}$ p $<$0.01.}}\n")
    file.write("\\end{tabular}} \n \\end{table} \n")






##############BALANCE WITH EXTRA
terms = ["Total Revenue per capita (log)", "Total Spending per capita (log)", "Health and Sanitation Spending per capita (log)", "Non-Health Spending per capita (log)", "Non-Health Social Spending per capita (log)", "Non-Social Spending per capita (log)", "Health Spending per capita - Total (log)", "Health Spending per capita - Own Resources (log)", "Health Spending per capita - Other Resources (log)", "Health Spending per capita - Personnel (log)", "Health Spending per capita - Investment (log)", "Health Spending per capita - Outsourced (3rd parties services) (log)", "Health Spending per capita - Admin, Management, others (log)"]
names = ["Total Revenues", "Total Spending", "&&&&\\\\ Health Spending", "Non-Health Spending", "Non-Health Social Spending", "Non-Social Spending", " \\midrule \\textbf{Panel B: SIOPS}&&&&\\\\ Total Health Spending", "&&&&\\\\ From Own Resources", "From Other Resources", "&&&&\\\\ Personnel", "Investment", "Outsourced (3\\textsuperscript{rd} party services)", "Admin, Management and Others"]
sheets=['finbra','finbra','finbra','finbra','finbra','finbra','siops','siops','siops','siops','siops','siops','siops']

with open('fiscalSpend_balance_noOutlier_noDataQuality.tex', 'w') as file:

    file.write("\\begin{table}[htpb!] \n \\centering \n \\caption{Fiscal Reactions -- Robustness to Exclusion of Data Quality Check} \\label{table:fiscal_ext} \n")
    file.write("\\scalebox{0.92}{\\begin{tabular}{lccccc} \\toprule \n & \\multicolumn{4}{c}{With Data Quality Control} & \\multicolumn{1}{c}{Without Data} \\\\ \n")
    file.write("& \\multicolumn{4}{c}{} & \\multicolumn{1}{c}{Quality Controls} \\\\ \cmidrule(r){2-5}\cmidrule(r){6-6}\n")
    file.write("& \\ \\ \\ \\  (1) \\ \\ \\ \\ ")
    file.write("& \\ \\ \\ \\  (2) \\ \\ \\ \\ ")
    file.write("& \\ \\ \\ \\  (3) \\ \\ \\ \\ ")
    file.write("& \\ \\ \\ \\  (4) \\ \\ \\ \\ ")
    file.write("& \\ \\ \\ \\  (5) \\ \\ \\ \\  \\\\ \\midrule \n")
    file.write("\\textbf{Panel A: FINBRA}&&&&&\\\\\n")
    for pos, select_term in enumerate(terms):
        t1 = names[pos]

        # Load the Excel file
        sheet_name = 'fiscal_response_'+sheets[pos]
        # Read the specific sheet
        df = pd.read_excel(file_path, sheet_name=sheet_name)
        # Extract coefficients (from odd rows) and standard errors (from even rows)
        coefficients = df.iloc[::2, 0:].reset_index(drop=True)
        std_errors = df.iloc[1::2, 0:].reset_index(drop=True)


        
        # Collect betas and standard errors for each specification
        betas = []
        ses = []
        Ns  = []
        print(pos)
        for spec in range(1, 6):
            beta = coefficients.loc[(coefficients['term'] == select_term) & (coefficients['spec'] == spec)].iloc[0, 1]
            beta = re.sub(r'\*\*\*|\*\*|\*', replace_stars, beta)
            se = std_errors.loc[(std_errors['term'] == select_term) & (std_errors['spec'] == spec)].iloc[0, 1]
            N = coefficients.loc[(coefficients['term'] == select_term) & (coefficients['spec'] == spec)].iloc[0, 2]
            beta=format_numbers(beta)
            se = format_numbers(se)
            betas.append(beta)
            Ns.append(f"{N}")
            ses.append(f"{se}")
            
        # Print the formatted output
        file.write(f"{t1} & " + " & ".join(betas) + " \\\\\n")
        file.write(" & " + " & ".join(ses) + " \\\\\n")
        if pos==5 or pos==12:
            print("adding obs")
            print(Ns)
            file.write("&&&&&\\\\ Observations (Each cell) &"+"&".join(Ns) + " \\\\\n")
    file.write("\\midrule \n Data Quality Control &Y&Y&Y&Y&N\\\\ \n ")
    file.write("Municipal FE \\& Time-State FE & Y&Y&Y&Y&Y\\\\ \n ")
    file.write("Baseline Socioeconomic Controls $\\times$ Time & N&Y&Y&Y&Y\\\\ \n ")
    file.write("Time-Varying Controls & N&N&Y&Y&Y\\\\ \n ")
    file.write("Fiscal Controls & N&N&N&Y&N\\\\ \\bottomrule \n ")
    file.write("\\multicolumn{6}{p{18.2cm}}{{\\footnotesize \\textbf{Notes:}  Refer to Notes to Table \\ref{table:fiscal}.  Identical models are presented, with an additional column removing data quality controls.   All other details follow those described in Table \\ref{table:fiscal}. $^{*}$ p$<$0.10; $^{**}$ p $<$ 0.05; $^{***}$ p $<$0.01.}}\n")
    file.write("\\end{tabular}} \n \\end{table} \n")


    
