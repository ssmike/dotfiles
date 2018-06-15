import json
import os
import copy

base = None
with open('build/compile_commands.json') as fin:
    base = json.load(fin)

result = []

seen = set()
exts = ['h', 'cpp', 'cc', 'cxx']

for flags in base:
    _dir = flags['file'].rsplit('/', maxsplit=1)[0]
    for file in os.listdir(_dir):
        full_path = os.path.join(_dir, file)
        if full_path in seen:
            continue
        seen.add(full_path)
        for ext in exts:
            if full_path.endswith(ext):
                _flags = copy.copy(flags)
                _flags["file"] = full_path
                result.append(_flags)

with open('compile_commands.json', 'w') as fout:
    json.dump(result, fout, ensure_ascii=True, indent=2)

