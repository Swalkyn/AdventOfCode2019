#!/usr/bin/env python

import argparse
import os
import datetime
import pyperclip

SCALA_TEMPLATE = "package day{}\n\n{}object Main extends App {{\n{}}}\n"

parser = argparse.ArgumentParser()
parser.add_argument("-d", "--day", type=int, help="specify the day, otherwise uses current date")
parser.add_argument("--with-input", action="store_true", help="create an input file an paste clipboard contents in it")

args = parser.parse_args()

day = datetime.date.today().day if args.day == None else args.day

src_dir = f"src/main/scala/day{day}/"
res_dir = f"src/main/resources/day{day}/"

os.makedirs(src_dir)
with open(src_dir + f"Main.scala", 'w') as src_file:
    if args.with_input:
        os.makedirs(res_dir)
        with open(res_dir + "input.txt", 'w') as input_file:
            input_file.write(pyperclip.paste())
            imports = "import scala.io.Source\n\n"
            input_path = res_dir + "input.txt"
            code = f'\tval input = Source.fromFile("{input_path}").getLines()\n\t\n\tprintln(input.toList)\n'
            src_file.write(SCALA_TEMPLATE.format(day, imports, code))
    else:
        src_file.write(SCALA_TEMPLATE.format(day, "", "\t\n"))

print(f"Day {day} created")
