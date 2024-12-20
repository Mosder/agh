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

def get_random_value(column, _):
    if len(column.possible_values) == 0:
        return None
    index = int(random() * len(column.possible_values))
    if column.unique:
        return column.possible_values.pop(index)
    return column.possible_values[index]

def get_value_gamble(column, other_values, base_function, default, probability):
    if random() * 100 < probability:
        return default
    return base_function(column, other_values)

def get_next_value(column, _):
    if len(column.possible_values) == 0:
        return None
    return column.possible_values.pop(0)

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

def load_possible_values_from_file(file_command_log_path, saved_commands):
    path = read_or_input("Input file path: ", file_command_log_path, saved_commands)
    while not isfile(path):
        path = read_or_input("Input CORRECT file path: ", file_command_log_path, saved_commands)
    file = open(path, "r")
    values = []
    for line in file:
        if line[-1] == "\n":
            line = line[:-1]
        values.append(f"N'{line}'")
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

def get_integers_range_possible_values(file_command_log_path, saved_commands):
    range_start = read_or_input("Input start of range: ", file_command_log_path, saved_commands)
    while not is_int(range_start):
        range_start = read_or_input("Input CORRECT start of range: ", file_command_log_path, saved_commands)
    range_end = read_or_input("Input end of range: ", file_command_log_path, saved_commands)
    while not is_int(range_end):
        range_end = read_or_input("Input CORRECT end of range: ", file_command_log_path, saved_commands)
    range_start, range_end = min_max(int(range_start), int(range_end))
    return [i for i in range(range_start, range_end+1)]

def get_random_64_string(length):
    characters = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"
    string = ""
    for _ in range(length):
        string += characters[int(random() * 64)]
    return string

def is_leap(year):
    return year % 4 == 0 and (year % 100 != 0 or year % 400 == 0)

def get_max_days(year):
    max_days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    if is_leap(year):
        max_days[1] = 29
    return max_days

def get_max_day(year, month):
    return get_max_days(year)[month-1]

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

def get_dates_range_possible_values(file_command_log_path, saved_commands):
    range_start = read_or_input("Input start of range (yyyy-mm-dd): ", file_command_log_path, saved_commands)
    while not is_date(range_start):
        range_start = read_or_input("Input CORRECT start of range (yyyy-mm-dd): ", file_command_log_path, saved_commands)
    range_end = read_or_input("Input end of range (yyyy-mm-dd): ", file_command_log_path, saved_commands)
    while not is_date(range_end):
        range_end = read_or_input("Input CORRECT end of range (yyyy-mm-dd): ", file_command_log_path, saved_commands)
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

def get_times_range_possible_values(file_command_log_path, saved_commands):
    range_start = read_or_input("Input start of range (hh:mm:ss): ", file_command_log_path, saved_commands)
    while not is_time(range_start):
        range_start = read_or_input("Input CORRECT start of range (hh:mm:ss): ", file_command_log_path, saved_commands)
    range_end = read_or_input("Input end of range (hh:mm:ss): ", file_command_log_path, saved_commands)
    while not is_time(range_end):
        range_end = read_or_input("Input CORRECT end of range (hh:mm:ss): ", file_command_log_path, saved_commands)
    step = read_or_input("Input step (hh:mm:ss): ", file_command_log_path, saved_commands)
    while not is_time(step):
        step = read_or_input("Input CORRECT step (hh:mm:ss): ", file_command_log_path, saved_commands)
    range_start, range_end = min_max(time_to_int(range_start), time_to_int(range_end))
    times = []
    for t in range(range_start, range_end+1, time_to_int(step)):
        times.append(int_to_time(t))
    return times

def date_and_time_to_datetime(date, time):
    return f"{date[:-1]}T{time[1:]}"

def get_datetime_possible_values(file_command_log_path, saved_commands):
    dates = get_dates_range_possible_values(file_command_log_path, saved_commands)
    times = get_times_range_possible_values(file_command_log_path, saved_commands)
    datetimes = []
    for d in dates:
        for t in times:
            datetimes.append(date_and_time_to_datetime(d, t))
    return datetimes

def get_datetime_after_random_duration(column, other_values):
    base_dt = other_values[column.value_function_args]
    base_dt_array = base_dt.split('T')
    duration = get_random_value(column, other_values)
    new_time = int_to_time(time_to_int(base_dt_array[1][:-1]) + time_to_int(duration[1:-1]))
    return date_and_time_to_datetime(base_dt_array[0] + "'", new_time)

def get_duration_function_args(file_command_log_path, saved_commands):
    dt_index = read_or_input("Input column number of datetime: ", file_command_log_path, saved_commands)
    while not is_int(dt_index):
        dt_index = read_or_input("Input CORRECT column number of datetime: ", file_command_log_path, saved_commands)
    return int(dt_index)-1

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

def get_money_range_possible_values(file_command_log_path, saved_commands):
    range_start = read_or_input("Input start of range (.2f): ", file_command_log_path, saved_commands)
    while not is_money(range_start):
        range_start = read_or_input("Input CORRECT start of range (.2f): ", file_command_log_path, saved_commands)
    range_end = read_or_input("Input end of range (.2f): ", file_command_log_path, saved_commands)
    while not is_money(range_end):
        range_end = read_or_input("Input CORRECT end of range (.2f): ", file_command_log_path, saved_commands)
    step = read_or_input("Input step (.2f): ", file_command_log_path, saved_commands)
    while not is_money(step):
        step = read_or_input("Input CORRECT step (.2f): ", file_command_log_path, saved_commands)
    range_start, range_end = min_max(money_to_int(range_start), money_to_int(range_end))
    moneys = []
    for m in range(range_start, range_end+1, money_to_int(step)):
        moneys.append(int_to_money(m))
    return moneys

def get_empty_set(file_command_log_path, saved_commands):
    return set()

def get_mail_function_args(file_command_log_path, saved_commands):
    name_index = read_or_input("Input column number of name: ", file_command_log_path, saved_commands)
    while not is_int(name_index):
        name_index = read_or_input("Input CORRECT column number of name: ", file_command_log_path, saved_commands)
    surname_index = read_or_input("Input column number of surname: ", file_command_log_path, saved_commands)
    while not is_int(surname_index):
        surname_index = read_or_input("Input CORRECT column number of surname: ", file_command_log_path, saved_commands)
    domain = read_or_input("Input domain: ", file_command_log_path, saved_commands)
    return int(name_index)-1, int(surname_index)-1, domain

def get_bits(file_command_log_path, saved_commands):
    percentage = read_or_input("Enter probability for 1 (in %): ", file_command_log_path, saved_commands)
    while not is_int(percentage) or int(percentage) < 0 or int(percentage) > 100:
        percentage = read_or_input("Enter CORRECT probability for 1 (in %): ", file_command_log_path, saved_commands)
    ones = int(percentage)
    return [1 for _ in range(ones)] + [0 for _ in range(100-ones)]

def get_null(_, __):
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
    "i2": ValueType("int from range of ints (inclusive) in order", get_integers_range_possible_values, get_next_value, None, False),
    "f": ValueType("random values from file", load_possible_values_from_file, get_random_value, None, True),
    "f2": ValueType("values from file in given order", load_possible_values_from_file, get_next_value, None, False),
    "d": ValueType("random date from a range of dates (inclusive)", get_dates_range_possible_values, get_random_value, None, True),
    "t": ValueType("random time from a range and given step", get_times_range_possible_values, get_random_value, None, True),
    "dt": ValueType("datetime (combines d and t)", get_datetime_possible_values, get_random_value, None, True),
    "dt2": ValueType("datetime from other datetime and duration from range", get_times_range_possible_values, get_datetime_after_random_duration, get_duration_function_args, False),
    "m": ValueType("random money from range of given step", get_money_range_possible_values, get_random_value, None, True),
    "ml": ValueType("get mail from name and surname (must be after name and surname)", get_empty_set, get_mail, get_mail_function_args, False),
    "b": ValueType("random bit (boolean) value", get_bits, get_random_value, None, False),
    "n": ValueType("fills column with nulls", get_null, get_random_value, None, False),
    "<other-command> g": ValueType("other value with chance at chosen default value", None, None, None, False)
}

def read_or_input(input_message, file_command_log_path, saved_commands):
    if len(saved_commands) > 0:
        return saved_commands.pop(0)
    f = open(file_command_log_path, "a")
    inp = input(input_message)
    f.write(inp+"\n")
    return inp

def get_column_info(column_number, file_command_log_path, saved_commands):
    print(f"Column {column_number+1}:")
    column_name = read_or_input("Column name: ", file_command_log_path, saved_commands)
    for command, pvt in POSSIBLE_VALUE_TYPES.items():
        print(f"{command} - {pvt.description}")
    value_type_command = read_or_input("Choose value type for column: ", file_command_log_path, saved_commands)
    vtc_array = value_type_command.split(" ")
    while vtc_array[0] not in POSSIBLE_VALUE_TYPES.keys():
        value_type_command = read_or_input("Choose CORRECT value type for column: ", file_command_log_path, saved_commands)
        vtc_array = value_type_command.split(" ")
    value_type = POSSIBLE_VALUE_TYPES[vtc_array[0]]
    if len(vtc_array) > 1 and vtc_array[1] == "g":
        default_value = read_or_input("Enter default value: ", file_command_log_path, saved_commands)
        probability = read_or_input("Enter probability (in %) for default value: ", file_command_log_path, saved_commands)
        while not is_int(probability) or int(probability) < 0 or int(probability) > 100:
            probability = read_or_input("Enter CORRECT probability (in %) for default value: ", file_command_log_path, saved_commands)
        base_function = value_type.get_value_function
        value_type.get_value_function = lambda col, vals: get_value_gamble(col, vals, base_function, default_value, int(probability))
    unique = False
    if value_type.can_be_unique:
        unique_command = read_or_input("Unique values? (N/y): ", file_command_log_path, saved_commands)
        if unique_command.lower() in ["yes", "y"]:
            unique = True
    possible_values = value_type.possible_values_generator(file_command_log_path, saved_commands)
    value_function_args = None
    if value_type.get_value_function_args:
        value_function_args = value_type.get_value_function_args(file_command_log_path, saved_commands)
    return Column(column_name, possible_values, unique, value_type.get_value_function, value_function_args)

def query_generator():
    file_output_path = input("File output name: ")
    file_command_log_path = file_output_path + ".log"
    saved_commands = []
    if isfile(file_command_log_path):
        f = open(file_command_log_path, "r")
        for line in f:
            if line[-1] == "\n":
                line = line[:-1]
            saved_commands.append(line)
        f.close()
    max_num_of_queries = int(read_or_input("Max number of queries to generate: ", file_command_log_path, saved_commands))
    table_name = read_or_input("Table name: ", file_command_log_path, saved_commands)
    num_of_columns = int(read_or_input("Number of columns: ", file_command_log_path, saved_commands))
    columns = []
    for i in range(num_of_columns):
        columns.append(get_column_info(i, file_command_log_path, saved_commands))
    queries = generate_insert_queries(max_num_of_queries, table_name, columns)
    save_queries(file_output_path, queries)

def address_generator():
    addresses = load_possible_values_from_file()
    for i in range(len(addresses)):
        a = addresses[i].split(", ")
        addresses[i] = (a[0][1:], a[3], a[2], a[-1][:-1])
    c = Column(None, addresses, True, None, None)
    file = open("addresses.sql", "w")
    for i in range(6202, 7502):
        ad = get_random_value(c, None)
        q = f"INSERT INTO addresses (studentID, street, city, zipCode, country) VALUES ({i},'{ad[0]}','{ad[1]}','{ad[2]}','{ad[3]}')\n"
        file.write(q)
    file.close()

if __name__ == "__main__":
    query_generator()
    # f = open("modules-old_webinars.sql", "r")
    # f2 = open("webinars-old.sql", "w")
    # c = 0
    # price = get_column_info(0)
    # has_price = get_column_info(1)
    # for line in f:
    #     c += 1
    #     n = line.split("'")[1]
    #     p1, p2 = price.get_value(None), has_price.get_value(None)
    #     p = float(p1) * p2
    #     f2.write(f"INSERT INTO webinars (webinarID, moduleID, name, price) VALUES ({c}, {c}, '{n}', {p})\n")

