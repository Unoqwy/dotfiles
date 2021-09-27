#!/bin/python3
# Bouygues Telecom interface to fetch data usage

import argparse
import sys

import json
import requests

endpoint = "https://api.bouyguestelecom.fr/contrats/{contract}/suivi-conso-mobile"

def query_data(access_token, contract):
    headers = {"Accept": "application/json", "Authorization": "Bearer {}".format(access_token)}
    response = requests.get(url = endpoint.format(contract=contract), headers=headers)
    data = response.json()
    return data

def parse_data(data):
    internet = data["compteursConsoDATA"][0]
    voice = data["compteursConsoVOIX"][0]
    return {
        "internet": {
            "bytes_used": internet["quantiteConsommee"],
            "bytes_total": internet["quantiteTotale"],
        },
        "voice": {
            "seconds_used": int(voice["quantiteConsommee"]),
            "seconds_total": int(voice["quantiteTotale"]),
        },
    }


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--token", help="Bearer access token", required=True)
    parser.add_argument("--contract", help="Contract ID of the phone line", required=True)
    args = parser.parse_args()

    raw_data = query_data(args.token, args.contract)
    if "error" in raw_data:
        print("Query Error: " + raw_data["error_description"], file=sys.stderr)
        sys.exit(1)
    data = parse_data(raw_data)
    print(json.dumps(data))
