#! /usr/bin/env python3

import os
import re

# Todo:
#   - Move to separate project and build wheel.
#   - Add cli arguments (input file, output, etc)
#   - Add tests (maybe Lettuce?)
#   - Add test case for no-argument functions
#   - Add test case for functions that end with a brace (control-flow statement, etc)

source_dir = os.path.dirname(__file__)
commands_file = os.path.join(source_dir, "commands.swift")

_identifier_start = "a-zA-Z_"
_identifier_char = _identifier_start + "0-9"
_identifier = f"(?:[{_identifier_start}][{_identifier_char}]*)"

def read_parameter_names(parameterSection):
    search = f"{_identifier}?\s*({_identifier})\s*:\s*{_identifier}"

    names = []
    for param in parameterSection.split(","):
        parameterMunch = re.search(search, param)

        if parameterMunch:
            names.append(parameterMunch.group(1))
    return names

def read_function(func):
    parameters = "\(([^\)]*)\)"

    search = f"func\s*({_identifier})\s*{parameters}\s*{{\s*"

    functionMunch = re.search(search, func)

    if functionMunch:
        func_name = functionMunch.group(1)
        parameters = functionMunch.group(2)

        param_names = read_parameter_names(parameters)

        return (func_name, param_names)

def lldb_param(name, num):
    return f"let {name} = %{num}"

with open(commands_file) as f:
    in_command = False
    name = ""
    lines = []
    num_params = 0

    for line in f:
        if not in_command:
            if line.startswith("func"):
                (func_name, param_names) = read_function(line)

                name = func_name
                lines = [lldb_param(name, num+1) for num, name in enumerate(param_names)]
                num_params = len(param_names)

                in_command = True
        else:
            if line.startswith("}"):
                # Put swift on one line with semicolons.
                # Wrap in a do block to avoid a return value that gets displayed by lldb.
                swift = "do { " + "; ".join(lines) + "; }"
                if 0 == num_params:
                    print(f"command alias {name} expr -l Swift -O -- {swift}")
                else:
                    param_regex = ("(.+) " * num_params).strip()
                    print(f"command regex {name} 's/{param_regex}/expr -l Swift -O -- {swift}/'")

                in_command = False
            else:
                lines.append(line.strip())
