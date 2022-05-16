#!/bin/python3
import yaml
import os


def dotenv_to_vars(dotenv):
    vars = {}
    for key, val in dotenv.items():
        key = key.replace("-", "_")
        if isinstance(val, dict):
            sub = dotenv_to_vars(val)
            for k, v in sub.items():
                vars[f"{key}_{k}"] = v
        else:
            vars[key] = val
    return vars


def load_dotenv_vars(path):
    with open(os.path.join(path, ".env.example"), "r") as fs:
        dotenv = yaml.safe_load(fs)
        defaults = dotenv_to_vars(dotenv)
    with open(os.path.join(path, ".env"), "r") as fs:
        dotenv = yaml.safe_load(fs)
        vars = dotenv_to_vars(dotenv)
    return defaults | vars

