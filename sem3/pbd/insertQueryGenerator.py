from os.path import isfile
from random import random

class Column:
    def __init__(self, column_name, possible_values, unique, get_value_function, value_function_args):
        self.column_name = column_name
        self.possible_values = possible_values
        self.unique = unique
        self.get_value_function = get_value_function
        self.value_function_args = value_function_args
    
    def get_value(self, other_values):
        return self.get_value_function(self, other_values)

def get_random_value(column, other_values):
    if len(column.possible_values) == 0:
        return None
    index = int(random() * len(column.possible_values))
    if column.unique:
        return column.possible_values.pop(index)
    return column.possible_values[index]

def get_mail(column, other_values):
    name_index, surname_index, domain = column.value_function_args
    name = other_values[name_index]
    surname = other_values[surname_index]
    mail = f"'{name[1].lower()}{surname[1:-1].lower()}@{domain}'"
    count = 1
    while mail in column.possible_values: # Use possible values as used mails (xd)
        count += 1
        mail = f"'{name[1].lower()}{surname[1:-1].lower()}{count}@{domain}'"
    column.possible_values.add(mail)
    return mail

class ValueType:
    def __init__(self, description, possible_values_generator, get_value_function, get_value_function_args, can_be_unique):
        self.description = description
        self.possible_values_generator = possible_values_generator
        self.can_be_unique = can_be_unique
        self.get_value_function = get_value_function
        self.get_value_function_args = get_value_function_args

def load_possible_values_from_file():
    path = input("Input file path: ")
    while not isfile(path):
        path = input("Input CORRECT file path: ")
    file = open(path, "r")
    values = []
    for line in file:
        if line[-1] == "\n":
            line = line[:-1]
        values.append(f"'{line}'")
    return values

def is_int(string):
    try: 
        int(string)
    except ValueError:
        return False
    else:
        return True

def min_max(val1, val2):
    if val1 <= val2:
        return val1, val2
    return val2, val1

def get_integers_range_possible_values():
    range_start = input("Input start of range: ")
    while not is_int(range_start):
        range_start = input("Input CORRECT start of range: ")
    range_end = input("Input end of range: ")
    while not is_int(range_end):
        range_end = input("Input CORRECT end of range: ")
    range_start, range_end = min_max(int(range_start), int(range_end))
    return [i for i in range(range_start, range_end+1)]

def is_leap(year):
    return year % 4 == 0 and (year % 100 != 0 or year % 400 == 0)

def get_max_days(year):
    max_days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    if is_leap(year):
        max_days[1] = 29
    return max_days

def get_max_day(year, month):
    return get_max_days(year)[month]

def is_correct_date(year, month, day):
    if year < 1800:
        return False
    if month < 1 or month > 12:
        return False
    if day < 1 or day > get_max_day(year, month):
        return False
    return True

def split_string_to_date(string):
    arr = string.split("-")
    return int(arr[0]), int(arr[1]), int(arr[2])

def is_date(string):
    year = month = day = None
    try: 
        year, month, day = split_string_to_date(string)
    except ValueError:
        return False
    else:
        if is_correct_date(year, month, day):
            return True
        return False

def min_max_date(date1, date2):
    y1, m1, d1 = split_string_to_date(date1)
    y2, m2, d2 = split_string_to_date(date2)
    if y1 < y2:
        return date1, date2
    elif y1 > y2:
        return date2, date1
    else:
        if m1 < m2:
            return date1, date2
        elif m1 > m2:
            return date2, date1
        else:
            if d1 < d2:
                return date1, date2
            return date2, date1

def ymd_to_string(year, month, day):
    if month < 10:
        month = f"0{month}"
    if day < 10:
        day = f"0{day}"
    return f"{year}-{month}-{day}"

def get_dates_range_possible_values():
    range_start = input("Input start of range (yyyy-mm-dd): ")
    while not is_date(range_start):
        range_start = input("Input CORRECT start of range (yyyy-mm-dd): ")
    range_end = input("Input end of range (yyyy-mm-dd): ")
    while not is_date(range_end):
        range_end = input("Input CORRECT end of range (yyyy-mm-dd): ")
    range_start, range_end = min_max_date(range_start, range_end)
    dates = [f"'{range_start}'"]
    y, m, d = split_string_to_date(range_start)
    y_end, m_end, d_end = split_string_to_date(range_end)
    max_days = get_max_days(y)
    while y != y_end or m != m_end or d != d_end:
        d += 1
        if d > max_days[m-1]:
            d = 1
            m += 1
            if m > 12:
                m = 1
                y += 1
                max_days = get_max_days(y)
        dates.append(f"'{ymd_to_string(y,m,d)}'")
    return dates

def is_correct_time(hr, mn, sc):
    if 0 <= hr <= 23 and 0 <= mn <= 59 and 0 <= sc <= 59:
        return True
    return False

def split_string_to_time(string):
    arr = string.split(":")
    return int(arr[0]), int(arr[1]), int(arr[2])

def is_time(string):
    hr = mn = sc = None
    try:
        hr, mn, sc = split_string_to_time(string)
    except ValueError:
        return False
    else:
        if is_correct_time(hr, mn, sc):
            return True
        return False

def time_to_int(date):
    hr, mn, sc = split_string_to_time(date)
    return sc + 60 * mn + 3600 * hr

def int_to_time(int_time):
    hr = int_time // 3600
    mn = (int_time % 3600) // 60
    sc = int_time % 60
    if hr < 10:
        hr = "0" + str(hr)
    if mn < 10:
        mn = "0" + str(mn)
    if sc < 10:
        sc = "0" + str(sc)
    return f"'{hr}:{mn}:{sc}'"

def get_times_range_possible_values():
    range_start = input("Input start of range (hh:mm:ss): ")
    while not is_time(range_start):
        range_start = input("Input CORRECT start of range (hh:mm:ss): ")
    range_end = input("Input end of range (hh:mm:ss): ")
    while not is_time(range_end):
        range_end = input("Input CORRECT end of range (hh:mm:ss): ")
    step = input("Input step (hh:mm:ss): ")
    while not is_time(step):
        step = input("Input CORRECT step (hh:mm:ss): ")
    range_start, range_end = min_max(time_to_int(range_start), time_to_int(range_end))
    times = []
    for t in range(range_start, range_end+1, time_to_int(step)):
        time.append(int_to_time(t))
    return times

def date_and_time_to_datetime(date, time):
    return f"{date[:-1]}T{time[1:]}"

def get_datetime_possible_values():
    dates = get_dates_range_possible_values()
    times = get_times_range_possible_values()
    datetimes = []
    for d in dates:
        for t in times:
            datetimes.append(date_and_time_to_datetime(d, t))
    return datetimes

def split_string_to_money(string):
    arr = string.split(".")
    return int(arr[0]), int(arr[1])

def is_correct_money(dollar, cent):
    if cent < 100:
        return True
    return False

def is_money(string):
    dollar = cent = None
    try:
        dollar, cent = split_string_to_money(string)
    except ValueError:
        return False
    else:
        if is_correct_money(dollar, cent):
            return True
        return False

def money_to_int(string):
    dollar, cent = split_string_to_money(string)
    return dollar * 100 + cent

def int_to_money(int_money):
    return f"{int_money // 100}.{int_money % 100}"

def get_money_range_possible_values():
    range_start = input("Input start of range (.2f): ")
    while not is_money(range_start):
        range_start = input("Input CORRECT start of range (.2f): ")
    range_end = input("Input end of range (.2f): ")
    while not is_money(range_end):
        range_end = input("Input CORRECT end of range (.2f): ")
    step = input("Input step (.2f): ")
    while not is_money(step):
        step = input("Input CORRECT step (.2f): ")
    range_start, range_end = min_max(money_to_int(range_start), money_to_int(range_end))
    moneys = []
    for m in range(range_start, range_end+1, money_to_int(step)):
        moneys.append(int_to_money(m))
    return moneys

def get_empty_set():
    return set()

def get_mail_function_args():
    name_index = input("Input column number of name: ")
    while not is_int(name_index):
        name_index = input("Input CORRECT column number of name: ")
    surname_index = input("Input column number of surname: ")
    while not is_int(surname_index):
        surname_index = input("Input CORRECT column number of surname: ")
    domain = input("Input domain: ")
    return int(name_index)-1, int(surname_index)-1, domain

def get_bits():
    percentage = input("Enter probability for 1")
    while not is_int(percentage) or int(percentage) < 0 or int(percentage) > 100:
        percentage = input("Enter CORRECT probability for 1")
    ones = int(percentage)
    return [1 for _ in range(ones)] + [0 for _ in range(100-ones)]

def get_null():
    return ["NULL"]

def generate_insert_queries(max_num_of_queries, table_name, columns):
    base_query = f"INSERT INTO {table_name} ("
    for col in columns:
        base_query += col.column_name + ","
    base_query = base_query[:-1] + ") VALUES ("
    queries = []
    for _ in range(max_num_of_queries):
        query = base_query
        values = []
        for col in columns:
            values.append(col.get_value(values))
        if None in values:
            break
        for val in values:
            query += f"{val},"
        query = query[:-1] + ")"
        queries.append(query)
    return queries

def save_queries(path, queries):
    if len(path) < 4 or path[-4:] != ".sql":
        path += ".sql"
    file = open(path, "w")
    for query in queries:
        file.write(query + "\n")
    file.close()

POSSIBLE_VALUE_TYPES = {
    "i": ValueType("random int from range of ints (inclusive)", get_integers_range_possible_values, get_random_value, None, True),
    "f": ValueType("random values from file", load_possible_values_from_file, get_random_value, None, True),
    "d": ValueType("random date from a range of dates (inclusive)", get_dates_range_possible_values, get_random_value, None, True),
    "t": ValueType("random time from a range and given step", get_times_range_possible_values, get_random_value, None, True),
    "dt": ValueType("datetime (combines d and t)", get_datetime_possible_values, get_random_value, None, True),
    "m": ValueType("random money from range of given step", get_money_range_possible_values, get_random_value, None, True),
    "ml": ValueType("get mail from name and surname (must be after name and surname)", get_empty_set, get_mail, get_mail_function_args, False),
    "b": ValueType("random bit (boolean) value", get_bits, get_random_value, None, False),
    "n": ValueType("fills column with nulls", get_null, get_random_value, None, False)
}

def get_column_info(column_number):
    print(f"Column {column_number+1}:")
    column_name = input("Column name: ")
    for command, pvt in POSSIBLE_VALUE_TYPES.items():
        print(f"{command} - {pvt.description}")
    value_type_command = input("Choose value type for column: ")
    while value_type_command not in POSSIBLE_VALUE_TYPES.keys():
        value_type_command = input("Choose CORRECT value type for column: ")
    value_type = POSSIBLE_VALUE_TYPES[value_type_command]
    unique = False
    if value_type.can_be_unique:
        unique_command = input("Unique values? (N/y): ")
        if unique_command.lower() in ["yes", "y"]:
            unique = True
    possible_values = value_type.possible_values_generator()
    value_function_args = None
    if value_type.get_value_function_args:
        value_function_args = value_type.get_value_function_args()
    return Column(column_name, possible_values, unique, value_type.get_value_function, value_function_args)

if __name__ == "__main__":
    file_output_path = input("File output name: ")
    max_num_of_queries = int(input("Max number of queries to generate: "))
    table_name = input("Table name: ")
    num_of_columns = int(input("Number of columns: "))
    columns = []
    for i in range(num_of_columns):
        columns.append(get_column_info(i))
    queries = generate_insert_queries(max_num_of_queries, table_name, columns)
    save_queries(file_output_path, queries)
    # addresses = load_possible_values_from_file()
    # for i in range(len(addresses)):
    #     a = addresses[i].split(", ")
    #     addresses[i] = (a[0][1:], a[3], a[2], a[-1][:-1])
    # c = Column(None, addresses, True, None, None)
    # file = open("addresses.sql", "w")
    # for i in range(6202, 7502):
    #     ad = get_random_value(c, None)
    #     q = f"INSERT INTO addresses (studentID, street, city, zipCode, country) VALUES ({i},'{ad[0]}','{ad[1]}','{ad[2]}','{ad[3]}')\n"
    #     file.write(q)
    # file.close()

