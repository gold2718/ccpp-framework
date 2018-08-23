#!/usr/bin/env python

"""
Create CCPP parameterization caps, host-model interface code,
physics suite runtime code, and CCPP framework documentation.
"""

# Python library imports
import argparse
import sys
import os
import os.path
import logging
# CCPP framework imports
from fortran_tools import parse_fortran_file
from host_model import HostModel
from host_cap import write_host_cap
from ccpp_suite import API, Suite
from parse_tools import initLog, setLogToStdout, setLogLevel
from parse_tools import CCPPError, ParseInternalError

## Init this now so that all Exceptions can be trapped
logger = initLog('ccpp_capgen')

###############################################################################
def is_xml_file(filename):
###############################################################################
    parts = os.path.basename(filename).split('.')
    return (len(parts) > 1) and (parts[-1].lower() == 'xml')

###############################################################################
def check_for_existing_file(filename, description, readable=True):
###############################################################################
    'Check for file existence and access, abort on error'
    if os.path.exists(filename):
        if readable:
            if not os.access(filename, os.R_OK):
                raise CCPPError("No read access to {}, '{}'".format(description, filename))
            # End if
        # End if (no else needed, checks all done
    else:
        raise CCPPError("{}, '{}', must exist".format(description, filename))
    # End if

###############################################################################
def check_for_writeable_file(filename, description):
###############################################################################
    if os.path.exists(filename) and not os.access(filename, os.W_OK):
        raise CCPPError("Cannot write {}, '{}'".format(description, filename))
    elif not os.access(os.path.dirname(filename), os.W_OK):
        raise CCPPError("Cannot write {}, '{}'".format(description, filename))
    # End if (else just return)

###############################################################################
def parse_command_line(args, description):
###############################################################################
    parser = argparse.ArgumentParser(description=description,
                                     formatter_class=argparse.RawTextHelpFormatter)

    parser.add_argument("--host-files", metavar='<host files filename>',
                        type=str, required=True,
                        help="""Comma separated list of host filenames to process
Filenames with a '.md' suffix are treated as host model metadata files
Other filenames are treated as containing a list of .md filenames""")

    parser.add_argument("--scheme-files", metavar='<scheme files filename>',
                        type=str, required=True,
                        help="""Comma separated list of scheme filenames to process
Filenames with a '.md' suffix are treated as scheme metadata files
Other filenames are treated as containing a list of .md filenames""")

    parser.add_argument("--suites", metavar='<Suite definition file(s)>',
                        type=str, required=True,
                        help="""Comma separated list of suite definition filenames to process
Filenames with a '.xml' suffix are treated as suite definition XML files
Other filenames are treated as containing a list of .xml filenames""")

    parser.add_argument("--preproc-directives",
                        metavar='VARDEF1[,VARDEF2 ...]', type=str, default=None,
                        help="Proprocessor directives used to correctly parse source files")

    parser.add_argument("--cap-pathlist", type=str,
                        metavar='<filename for list of cap filenames>',
                        default="capfiles.txt",
                        help="Filename for list of generated cap files")

    parser.add_argument("--output-root", type=str,
                        metavar='<directory for generated files>',
                        default=os.getcwd(),
                        help="directory for generated files")

    parser.add_argument("--generate-host-cap",
                        action='store_true', default=True,
                        help="Generate a host cap with correct API calling sequence")

    parser.add_argument("--generate-docfiles",
                        metavar='HTML | Latex | HTML,Latex', type=str,
                        help="Generate LaTeX and/or HTML documentation")

    parser.add_argument("--verbose", action='count',
                        help="Log more activity, repeat for increased output")
    pargs = parser.parse_args(args)
    return pargs

###############################################################################
def read_pathnames_from_file(pathsfile):
###############################################################################
    'Read path names from <pathsfile> and return them as a list'
    # We want to end up with absolute paths, treat <pathsfile> as root location
    root_path = os.path.dirname(os.path.abspath(pathsfile))
    pdesc = 'pathname in {}'.format(pathsfile)
    pathnames = list()
    with open(pathsfile, 'r') as infile:
        for line in infile.readlines():
            path = line.strip()
            # Skip blank lines and lines which appear to start with a comment.
            if (len(path) > 0) and (path[0] != '#') and (path[0] != '!'):
                # Check for an absolute path
                if not os.path.isabs(path):
                    # Assume relative pathnames are relative to pathsfile
                    path = os.path.normpath(os.path.join(root_path, path))
                # End if
                check_for_existing_file(path, pdesc)
                pathnames.append(path)
            # End if (else skip blank or comment line)
        # End for
    # End with open
    return pathnames

###############################################################################
def create_file_list(files, suffices, file_type):
###############################################################################
    master_list = list()
    file_list = [x.strip() for x in files.split(',')]
    suffix_list = [x.strip() for x in suffices.split(',')]
    for filename in file_list:
        check_for_existing_file(filename, '{} pathnames file'.format(file_type))
        suffix = os.path.basename(filename).split('.')[-1]
        if suffix in suffix_list:
            master_list.append(os.path.abspath(filename))
        else:
            master_list.extend(read_pathnames_from_file(filename))
        # End if
    # End for
    return master_list

###############################################################################
def parse_host_model_files(host_filenames, preproc_defs, logger):
###############################################################################
    """
    Gather information from host files (e.g., DDTs, registry) and
    return a host model object with the information.
    """
    mheaders = {}
    xml_files = list()
    for filename in host_filenames:
        if is_xml_file(filename):
            # We have to process XML files after processing Fortran files
            # since the Fortran files may define DDTs used by registry files.
            xml_files.append(filename)
        else:
            hheaders = parse_fortran_file(filename, preproc_defs==preproc_defs, logger=logger)
            for header in hheaders:
                if header.title in mheaders:
                    raise CCPPError("Duplicate DDT, {}, found in {}".format(header.title, filename))
                else:
                    mheaders[header.title] = header
                # End if
            # End for
        # End if
    # End for
    host_model = None
    for file in xml_files:
        host_model = HostModel.parse_host_registry(file, logger, mheaders,
                                                   host_model=host_model)
    # End for
    return host_model

###############################################################################
def parse_scheme_files(scheme_filenames, preproc_defs, logger):
###############################################################################
    """
    Gather information from scheme files (e.g., init, run, and finalize
    methods) and return resulting dictionary.
    """
    mheaders = list()
    for filename in scheme_filenames:
        logger.info('Reading CCPP schemes from {}'.format(filename))
        sheaders = parse_fortran_file(filename, preproc_defs==preproc_defs)
        mheaders.append(sheaders)
    # End for
    return mheaders

###############################################################################
def _main_func():
###############################################################################
    args = parse_command_line(sys.argv[1:], __doc__)
    verbosity = args.verbose
    if verbosity > 1:
        setLogLevel(logger, logging.DEBUG)
    elif verbosity > 0:
        setLogLevel(logger, logging.INFO)
    # End if
    # We need to create three lists of files, hosts, schemes, and SDFs
    # XXgoldyXX: Note that host_files arg will change to '.md' after conversion
    fort_suffices = 'F90,f90,F,f'
    host_files = create_file_list(args.host_files, fort_suffices+',xml', 'Host')
    scheme_files = create_file_list(args.scheme_files, fort_suffices, 'Scheme')
    sdfs = create_file_list(args.suites, 'xml', 'Suite')
    # Make sure we know where output is going
    output_dir = os.path.abspath(args.output_root)
    if os.path.abspath(args.cap_pathlist):
        cap_output_file = args.cap_pathlist
    else:
        cap_output_file = os.path.abspath(os.path.join(output_dir, args.cap_pathlist))
    # End if
    preproc_defs = args.preproc_directives
    gen_hostcap = args.generate_host_cap
    gen_docfiles = args.generate_docfiles
    ## A few sanity checks
    ## Make sure output directory is legit
    if os.path.exists(output_dir):
        if not os.path.isdir(output_dir):
            raise CCPPError("output-root, '{}', is not a directory".format(args.output_root))
        elif not os.access(output_dir, os.W_OK):
            raise CCPPError("Cannot write files to output-root ({})".format(args.output_root))
        # End if (output_dir is okay)
    else:
        # Try to create output_dir (let it crash if it fails)
        os.makedirs(output_dir)
    # End if
    # Make sure we can create output file lists
    if not os.path.isabs(cap_output_file):
        cap_output_file = os.path.normpath(os.path.join(output_dir, cap_output_file))
    # End if
    check_for_writeable_file(cap_output_file, "Cap output file")
    ##XXgoldyXX: Temporary warning
    if gen_docfiles:
        raise CCPPError("--gen-docfiles not yet supported")
    # End if
    # First up, handle the host files
    host_model = parse_host_model_files(host_files, preproc_defs, logger)
    # Next, parse the scheme files
    scheme_headers = parse_scheme_files(scheme_files, preproc_defs, logger)
    logger.debug("headers = {}".format([host_model._ddt_defs[x].title for x in host_model._ddt_defs.keys()]))
    logger.debug("variables = {}".format([x.get_prop_value('local_name') for x in host_model.variable_list()]))
    logger.debug("schemes = {}".format([[x._table_title for x in y] for y in scheme_headers]))
    # Finally, we can get on with writing suites
    ccpp_api = API(sdfs, host_model, scheme_headers, logger)
    cap_filenames = ccpp_api.write(output_dir, logger)
    if gen_hostcap:
        # Create a cap file
        hcap_filename = write_host_cap(host_model, ccpp_api, output_dir, logger)
    else:
        hcap_filename = None
    # End if
    with open(cap_output_file, 'w') as cap_names:
        for path in cap_filenames:
            cap_names.write('{}\n'.format(path))
        # End for
        if hcap_filename is not None:
            cap_names.write('{}\n'.format(hcap_filename))
        # End if
    # End with

###############################################################################

if __name__ == "__main__":
    try:
        _main_func()
        sys.exit(0)
    except ParseInternalError as pie:
        logger.exception(pie)
        sys.exit(-1)
    except CCPPError as ca:
        if logger.getEffectiveLevel() <= logging.DEBUG:
            logger.exception(ca)
        else:
            logger.error(ca)
        # End if
        sys.exit(-1)
    # End try
