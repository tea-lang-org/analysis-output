import tea
import os
import json
import pandas as pd
from scipy import stats  # Stats library used
import statsmodels.api as sm
from statsmodels.formula.api import ols
from statsmodels.stats.anova import AnovaRM


base_url = 'https://homes.cs.washington.edu/~emjun/tea-lang/datasets/'
uscrime_data_path = 0
states_path = 1
cats_path = 2
cholesterol_path = 3
soya_path = 4
co2_path = 5
exam_path = 6
liar_path = 7
pbcorr_path = 8
spider_path = 9
drug_path = 10
alcohol_path = 11
ecstasy_path = 12
goggles_path = 13
goggles_dummy_path = 14
data_paths = [uscrime_data_path, states_path, cats_path, cholesterol_path, soya_path, co2_path, exam_path,
              liar_path, pbcorr_path, spider_path, drug_path, alcohol_path, ecstasy_path, goggles_path, goggles_dummy_path]
file_names = ['UScrime.csv', 'statex77.csv', 'catsData.csv', 'cholesterol.csv', 'soya.csv', 'co2.csv', 'exam.csv', 'liar.csv',
              'pbcorr.csv', 'spiderLong.csv', 'drug.csv', 'alcohol.csv', 'ecstasy.csv', 'gogglesData.csv', 'gogglesData_dummy.csv']

def test_load_data():
    global base_url, data_paths, file_names
    global drug_path

    for i in range(len(data_paths)):
        csv_name = file_names[i]
        csv_url = os.path.join(base_url, csv_name) # Fetch individual spreadsheets 
        data_paths[i] = tea.download_data(csv_url, csv_name)


def all_cors(var_0, var_1):
    results = {}

    # Pearson
    pearson = stats.pearsonr(var_0, var_1)
    results["pearson"] = pearson

    # Pointbiserial
    pointbiserial = stats.pointbiserialr(var_0, var_1)
    results["pointbiserial"] = pointbiserial

    # Kendall Tau
    tau = stats.kendalltau(var_0, var_1)
    results["tau"] = tau

    # Spearman Rho
    rho = stats.spearmanr(var_0, var_1)
    results["rho"] = rho

    return results

def test_all_corrs():
    tests = {
        'pearson': data_paths[states_path],
        'spearman': data_paths[liar_path],
        'kendall': data_paths[liar_path],
        'point_biserial': data_paths[pbcorr_path]
    }

    test_vars = {
        'pearson': ['Illiteracy', 'Life Exp'],
        'spearman': ['Position', 'Creativity'],
        'kendall': ['Position', 'Creativity'],
        'point_biserial': ['time', 'gender']
    }

    try:
        open("./Outputs/correlation_tests.txt", "w").close()
    except:
        pass # File not yet created, no need to overwrite it 

    with open("./Outputs/correlation_tests.txt", "a") as f:
        for tutorial_test, file_path in tests.items():
            df = pd.read_csv(file_path)

            variables = test_vars[tutorial_test]
            var_0_name = variables[0]
            var_1_name = variables[1]

            var_0 = df[var_0_name]
            var_1 = df[var_1_name]

            print(tutorial_test)
            f.write(tutorial_test)
            results = all_cors(var_0, var_1)
            print(results)
            f.write(json.dumps(results))
            print('\n---------')
            f.write('\n')


def all_bivariate(group_0, group_1):
    tests = [stats.ttest_ind, stats.ttest_rel,
             stats.mannwhitneyu, stats.wilcoxon]
    results = {}

    for test in tests:
        res = None
        try:
            res = test.__call__(group_0, group_1)
        except:
            # import pdb; pdb.set_trace()
            res = -1
        results[test.__name__] = res

    # Welch's
    try:
        welchs_t = stats.ttest_ind(group_0, group_1, equal_var=False)
    except:
        welchs_t = -1
    results["welchs_t"] = welchs_t
    return results


def test_all_bivariate():
    tests = {
        'students_t': data_paths[uscrime_data_path],
        'paired_t': data_paths[spider_path],
        'wilcoxon_signed_rank': data_paths[alcohol_path],
        'welchs': data_paths[uscrime_data_path]
    }

    test_vars = {
        'students_t': ['So', 'Prob'],
        'paired_t': ['Group', 'Anxiety'],
        'wilcoxon_signed_rank': ['day', 'value'],
        'welchs': ['So', 'Prob']
    }

    try:
        open("./Outputs/bivariate_tests.txt", "w").close()
    except:
        pass # File not yet created, no need to overwrite it 

    with open("./Outputs/bivariate_tests.txt", "a") as f:
        for tutorial_test, file_path in tests.items():
            df = pd.read_csv(file_path)

            variables = test_vars[tutorial_test]
            var_0_name = variables[0]
            var_1_name = variables[1]

            groups = df[var_0_name].unique()
            assert(len(groups) == 2)

            data = []
            for group in groups:
                d = df[var_1_name][df[var_0_name] == group]
                data.append(d)

            print(tutorial_test)
            f.write(tutorial_test)
            results = all_bivariate(data[0], data[1])
            print(results)
            f.write(json.dumps(results))
            print('\n---------')
            f.write('\n');


def _is_interaction_unique(interactions, inter):
    for existing_inter in interactions:
        variables = inter.split('*')
        # Check if the varibles in inter already exist in another interaction
        exists = [(v in existing_inter) for v in variables]
        # If all of the variables in inter already exist, this means
        # the interaction is not unique!
        return not all(exists)


def f_test(x_name, y_name, df):
    # F-test, Factorial ANOVA
    formula = ols(f"{y_name} ~ C({x_name})", data=df)
    model = formula.fit()
    res = sm.stats.anova_lm(model, type=2)
    return res


def factorial(xs, y, df):
    # assert(len(y) == 0)
    formula = f"{y} ~ "

    for i in range(len(xs)):
        x = xs[i]
        formula += f"C({x})"

        if i < len(xs) - 1:
            formula += " + "

    # Add the interactions
    interactions = []
    for i in range(len(xs)):
        x_i = xs[i]
        inter = f"C({x_i})"
        for j in range(len(xs)):
            if i != j:
                x_j = xs[j]
                inter += " * " + f"C({x_j})"
                interactions.append(inter)

                if _is_interaction_unique(interactions, inter):
                    formula += " + " + inter

    ols_formula = ols(formula, data=df)
    model = ols_formula.fit()
    return sm.stats.anova_lm(model, type=2)


def kruskall_wallis(xs, y, df):
    results = {}
    if len(xs) == 1:
        for x in xs:
            data = []
            groups = df[x].unique()
            for group in groups:
                d = df[y][df[x] == group]
                data.append(d)
            results[x] = stats.kruskal(*data)

    else:
        results = -1

    return results


def friedman(xs, y, df):
    results = {}
    if len(xs) == 1:
        for x in xs:
            data = []
            groups = df[x].unique()
            # import pdb; pdb.set_trace()
            for group in groups:
                d = df[y][df[x] == group]
                data.append(d)
            if (len(groups) >= 3):
                res = stats.friedmanchisquare(*data)
            else:
                res = -1
            results[x] = res
    else:
        results = -1

    return results


def rm_one_way(xs, y, key, df):
    
    aovrm2way = AnovaRM(df, depvar=y, subject=key,
                        within=xs, aggregate_func='mean')

    res2way = aovrm2way.fit()

    return str(res2way)


def all_multivariate(xs, y, key, df):

    results = {}

    assert(len(y) == 1)
    y = y[0]

    # F test
    if len(xs) == 1:
        x_name = xs[0]

        results['f_test'] = f_test(x_name, y, df).to_json()

    # Factorial ANOVA
    results['factorial_ANOVA'] = factorial(xs, y, df).to_json()

    # Repeated Measures
    if key:
        results['rm_one_way_ANOVA'] = rm_one_way(xs, y, key, df)

    # Kruskall-Wallis
    results['kruskall_wallis'] = kruskall_wallis(xs, y, df)

    # Friedman
    results['friedman'] = friedman(xs, y, df)

    return results


def test_all_multivariate():
    tests = {
        'f_test': data_paths[cholesterol_path],
        'kruskall_wallis': data_paths[soya_path],
        'rm_one_way': data_paths[co2_path],
        'factorial_anova': data_paths[goggles_path],
        'two_way_anova': data_paths[co2_path]
    }

    # Y var comes at the end!
    test_vars = {
        'f_test': ['trt', 'response'],
        'kruskall_wallis': ['Soya', 'Sperm'],
        'rm_one_way': ['conc', 'uptake', 'Plant'],
        'factorial_anova': ['gender', 'alcohol', 'attractiveness'],
        'two_way_anova': ['conc', 'Type', 'uptake']
    }

    try:
        open("./Outputs/multivariate_tests.txt", "w").close()
    except:
        pass # File not yet created, no need to overwrite it 

    with open("./Outputs/multivariate_tests.txt", "a") as f:
        for tutorial_test, file_path in tests.items():
            df = pd.read_csv(file_path)

            variables = test_vars[tutorial_test]

            key = ''
            if tutorial_test == 'rm_one_way':
                key = variables[2]
                variables = [v for i, v in enumerate(variables) if i < 2]
            # import pdb; pdb.set_trace()

            xs = []
            y = []
            for i in range(len(variables)):
                if i < len(variables) - 1:
                    xs.append(variables[i])
                if i == len(variables) - 1:
                    y.append(variables[i])

            print(tutorial_test)
            f.write(tutorial_test)
            results = all_multivariate(xs, y, key, df)
            print(results)
            f.write(json.dumps(results))
            print('\n---------')
            f.write('\n')


def chi_square(var_0, var_1, df):
    var_0_groups = df[var_0].unique()
    var_1_groups = df[var_1].unique()

    contingency_table = []
    for group_0 in var_0_groups:
        table_row = []
        for group_1 in var_1_groups:
            data = df[var_1][(df[var_0] == group_0) & (df[var_1] == group_1)]
            table_row.append(len(data))

        contingency_table.append(table_row)

    return stats.chi2_contingency(contingency_table, correction=False)


def all_proportions(var_0, var_1, df):
    results = {}

    # Chi Square
    res = str(chi_square(var_0, var_1, df))

    results['chi_square'] = res

    return results


def test_proportions():
    tests = {
        'chi_square': data_paths[cats_path]
    }

    test_vars = {
        'chi_square': ['Training', 'Dance']
    }

    try:
        open("./Outputs/proportions_tests.txt", "w").close()
    except:
        pass # File not yet created, no need to overwrite it 

    with open("./Outputs/proportions_tests.txt", "a") as f:
        for tutorial_test, file_path in tests.items():
            df = pd.read_csv(file_path)

            variables = test_vars[tutorial_test]
            assert(len(variables) == 2)
            var_0 = variables[0]
            var_1 = variables[1]

            # import pdb; pdb.set_trace()

            print(tutorial_test)
            f.write(tutorial_test)
            results = all_proportions(var_0, var_1, df)
            print(results)
            f.write(results['chi_square'])
            print('\n---------')
            f.write('\n')

if __name__ == "__main__":
    # Ensure user is in /scripts/Stats_Model before executing statsmodel.py

    test_load_data() # Must run in order to create path resolution
    # test_all_corrs() 
    # test_all_bivariate()
    # test_all_multivariate()
    test_proportions()