import re
import platform
import sys
import os

def trim(string: str):
    return string.lstrip().rstrip()

def trim_list(l: list):
    i = 0
    j = len(l)

    while i < j and not trim(l[i]):
        i += 1
    
    l = l[i:]

    j = len(l) - 1
    i = 0

    while j >= i and not trim(l[j]):
        j -= 1

    l = l[:j+1]
    return l

filepath_re = r'([A-Za-z]:(\\|/))?((\w|\.)+((\\|/)(\w|\.)+)*)' if platform.system() == 'Windows' \
         else r'/?((\w|\.)+((\\|/)(\w|\.)+)*)'

def get_full_path(prefix: str, filepath: str):
    if os.path.isabs(filepath):
        return filepath
    else:
        return os.path.abspath(os.path.join(prefix, filepath))

def is_system_include(string: str):
    return bool(re.match(r'( )*#( )*include( )+<{}>'.format(filepath_re), string))

def is_local_include(string: str):
    return bool(re.match(r'( )*#( )*include( )+"{}"'.format(filepath_re), string))

def extract_filepath(src_include: str, include: str):
    return get_full_path(
        os.path.dirname(src_include),
        trim(re.sub(r'( )*#( )*include( )+', '', include).replace('"', ''))
    )

def is_pargma_once(string: str):
    return bool(re.match(r'#( )*pragma( )+once', string))

ifndef_re = r'( )*#( )*ifndef( )+'
ifndefined_re = r'( )*#( )*if( )+!( )*defined\(( )*'
define_re = r'( )*#( )*define( )+'

def is_header_guard_ifndef(string: str):
    is_ifndef = bool(re.match(ifndef_re + r'[A-Za-z\d_]+', string))
    is_ifndefined = bool(re.match(ifndefined_re + r'[A-Za-z\d_]+( )*\)', string))

    if is_ifndef:
        guard_text = re.sub(ifndef_re, string, '')
    elif is_ifndefined:
        guard_text = re.sub(is_ifndefined, string, '')
        guard_text = guard_text.rsplit()[:-1] # remove last ')' // if !defined(<guard_text>)
    else:
        guard_text = ''

    is_hguard = is_ifndef or is_ifndefined

    return is_hguard, guard_text


def is_header_guard_define(ifndef_text: str, string: str):
    return bool(re.match(define_re + ifndef_text, string))

cycled_headers = []
header_stack = []

system_headers = []
headers = {}

is_errors = False

class Header:
    def __init__(self, filepath: str):
        self._filepath = filepath
        self._lines = []
        self._includes = []
        self._excluded_lines = []
        self._is_processed = False

    def __eq__(self, rhs):
        if isinstance(rhs, Header):
            return self._filepath == rhs._filepath
        else:
            return False

    @property
    def filepath(self):
        return self._filepath

    @property
    def lines(self):
        return ['\n'] + trim_list(self._lines) + ['\n']

    @property
    def length(self):
        return len(self.lines)

    @lines.setter
    def lines(self, lines):
        self._lines = lines

    @property
    def includes(self):
        return self._includes

    def include_line(self, include: str):
        for value in self._excluded_lines:
            if value['type'] == 'include':
                if value['path'] == include:
                    return value['index']+1

    def add_include(self, line_index: int, filepath: str):
        self._includes.append(filepath)

        self._excluded_lines.append({
            'index': line_index,
            'type': 'include',
            'path': filepath
        })

    def exclude_line(self, index: int):
        self._excluded_lines.append({
            'index': index,
            'type': 'exclude'
        })

    @property
    def is_processed(self):
        return self._is_processed
    
    def process(self):
        if not self.is_processed:
            offset = 0
            for excluded in sorted(self._excluded_lines, key=lambda d: d['index']):
                i = excluded['index'] + offset
                if excluded['type'] == 'exclude':
                    self._lines = self._lines[:i] + self._lines[i+1:]
                    offset -= 1
                else:
                    header = headers[excluded['path']]
                    if header.is_processed:
                        self._lines = self._lines[:i] + self._lines[i+1:]
                        offset -= 1
                    else:
                        header.process()
                        self._lines = self._lines[:i] + header.lines + self._lines[i+1:]

                        if not header.lines:
                            offset -= 1
                        else:
                            offset += header.length - 1

            self._is_processed = True


def add_system_header(header: str):
    if not header in system_headers:
        system_headers.append(header)

def add_header(header: Header):
    headers[header.filepath] = header

def fill_includes(filepath: str):
    filepath = os.path.abspath(filepath)
    header = Header(filepath)
    lines = []
    is_guard_expected = True
    is_guard_found = False
    is_ifndef_found = False
    ifndef_text = ''
    is_headers_expected = True

    if header in header_stack:
        cycle_start = header_stack.index(header)
        cycle = header_stack[cycle_start:]
        cycle.append(header)
        cycled_headers.append(cycle)
        return

    with open(filepath) as file:
        for index, line in enumerate(file):
            trim_line = trim(line)
            if is_headers_expected and trim_line and not trim_line.startswith('//'):
                if is_guard_expected:
                    if not is_guard_found:
                        if is_pargma_once(line):
                            is_guard_found = True
                            is_guard_expected = False
                        elif not is_ifndef_found:
                            is_ifndef_found, ifndef_text = is_header_guard_ifndef(line)
                        else:
                            is_guard_found = is_header_guard_define(ifndef_text, line)
                            is_guard_expected = not is_guard_found
                else:
                    is_guard_found = False
                    is_ifndef_found = False

                if not is_guard_found and not is_ifndef_found:
                    if is_system_include(line):
                        add_system_header(trim(line) + '\n')
                        header.exclude_line(index)
                    elif is_local_include(line):
                        local_path = extract_filepath(filepath, line)
                        if not os.path.exists(local_path):
                            print('{}:{} file not found "{}"'.format(header.filepath, index+1, local_path))
                            global is_errors
                            is_errors = True
                            return

                        header.add_include(index, os.path.abspath(local_path))
                    else:
                        is_headers_expected = False

            lines.append(line)
        header.lines = lines

        # >>   
        header_stack.append(header)
        add_header(header)
        
        for filepath in header.includes:
            fill_includes(filepath)
        header_stack.pop()
        # <<

def get_license_comment():
    if not os.path.exists('LICENSE'):
        return []

    license_text = ''
    license_lines = []
    with open('LICENSE') as license:
        license_text = license.read()
    license_lines = license_text.split('\n')
    for i in range(len(license_lines)):
        license_lines[i] = "// " + license_lines[i] + '\n'
    return license_lines + ['\n']


if __name__ == "__main__":
    if len(sys.argv) != 4 or sys.argv[2] != '-o':
        print("Usage: maheo.py <main_header.h> -o <output_header>")
        sys.exit(-1)
    
    if(sys.argv[1] == sys.argv[3]):
        print("Can't expand header to the file with the same name")
        sys.exit(-1)

    if not os.path.exists(sys.argv[1]):
        print('"{}": file not found'.format(sys.argv[1]))
        sys.exit(-1)

    main_header = os.path.abspath(sys.argv[1])
    output_header = sys.argv[3]

    fill_includes(main_header)

    if cycled_headers:
        print("Cycles were found:")
        for cycle in cycled_headers:
            i = 0
            end = len(cycle)-1

            while i < end:
                header = cycle[i]
                
                print('{}{}:{}'.format(
                    '\t' if i != 0 else '',
                    header.filepath, 
                    header.include_line(cycle[i+1].filepath))
                )
                
                i += 1
            print('\t{}'.format(cycle[0].filepath))
            print()
        sys.exit(-2)
    
    if is_errors:
        sys.exit(-2)

    headers[main_header].process() # process all

    library_name = os.path.basename(sys.argv[3]).upper()
    library_name = library_name[:library_name.index('.')]

    _, ext = os.path.splitext(output_header)
    ext = ext[1:].upper()
    guard = '{}_{}'.format(library_name, ext)

    ifndef_line = '#ifndef ' + guard + '\n'
    define_line = '#define ' + guard + '\n'
    endif_line = '#endif // ' + guard

    final_lines = get_license_comment() \
        + [ifndef_line] \
        + [define_line] \
        + ['\n'] \
        + system_headers \
        + headers[main_header].lines \
        + ['\n\n'] \
        + [endif_line]

    with open(output_header, 'w') as out:
        out.writelines(final_lines)