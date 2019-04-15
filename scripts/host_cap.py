#!/usr/bin/env python

"""
Parse a host-model registry XML file and return the captured variables.
"""

# Python library imports
from __future__ import print_function
import sys
import os
import os.path
import subprocess
import xml.etree.ElementTree as ET
# CCPP framework imports
from ccpp_suite    import COPYRIGHT, KINDS_MODULE, CCPP_STATE_MACH, API
from metavar       import Var, VarDictionary, CCPP_VAR_LOOP_SUBSTS
from fortran_tools import FortranWriter
from parse_tools   import CCPPError, ParseSource, ParseContext

###############################################################################
header = '''
!>
!! @brief Auto-generated cap for {host_model} calls to CCPP API
!!
!
module {module}

'''

preamble='''
   implicit none
   private

'''

subhead = '''
   subroutine {host_model}_ccpp_physics_{stage}({api_vars})
'''

subfoot = '''
   end subroutine {host_model}_ccpp_physics_{stage}
'''

footer = '''
end module {module}
'''

__api_source__ = ParseSource("CCPP_API", "MODULE",
                             ParseContext(filename="host_cap.F90"))

__suite_name_var__ = Var({'local_name':'suite_name',
                          'standard_name':'suite_name',
                          'intent':'in', 'type':'character',
                          'kind':'len=*', 'units':'',
                          'dimensions':'()'}, __api_source__)

__suite_part_var__ = Var({'local_name':'suite_part',
                          'standard_name':'suite_part',
                          'intent':'in', 'type':'character',
                          'kind':'len=*', 'units':'',
                          'dimensions':'()'}, __api_source__)

###############################################################################
def suite_part_list(suite, stage):
###############################################################################
    "Return a list of all the suite parts for this stage"
    run_stage = stage == 'run'
    if run_stage:
        spart_list = list()
        for spart in suite.groups:
            if suite.is_run_group(spart):
                spart_list.append(spart)
            # End if
        # End for
    else:
        spart_list = [suite.phase_group(stage)]
    # End if
    return spart_list

###############################################################################
def write_host_cap(host_model, api, output_dir, logger):
###############################################################################
    module_name = "{}_ccpp_cap".format(host_model.name)
    cap_filename = os.path.join(output_dir, '{}.F90'.format(module_name))
    if logger is not None:
        msg = 'Writing CCPP Host Model Cap for {} to {}'
        logger.info(msg.format(host_model.name, cap_filename))
    # End if
    with FortranWriter(cap_filename, 'w') as cap:
        cap.write(COPYRIGHT, 0)
        cap.write(header.format(host_model=host_model.name,
                                module=module_name), 0)
        cap.write('   use {kinds}'.format(kinds=KINDS_MODULE), 1)

        modules = host_model.variable_locations()
        mlen = max([len(x[0]) for x in modules])
        max_suite_len = 0
        for suite in api.suites:
            max_suite_len = max(max_suite_len, len(suite.module))
        # End for
        for mod in modules:
            mspc = (mlen - len(mod[0]))*' '
            cap.write("use {}, {}only: {}".format(mod[0], mspc, mod[1]), 1)
        # End for
        cap.write(preamble.format(host_model=host_model.name), 1)
        # CCPP_STATE_MACH.transitions represents the host CCPP interface
        for stage in CCPP_STATE_MACH.transitions():
            cap.write("public :: {host_model}_ccpp_physics_{stage}".format(host_model=host_model.name, stage=stage), 1)
        # End for
        api.declare_inspection_interfaces(cap)
        cap.write('\ncontains\n', 0)
        for stage in CCPP_STATE_MACH.transitions():
            # Create a dict of local variables for stage
            host_local_vars = VarDictionary("{}_{}".format(host_model.name,
                                                           stage))
            # Create part call lists
            # Look for any loop-variable mismatch
            spart_list = suite_part_list(suite, stage)
            for spart in spart_list:
                spart_args = spart.call_list.variable_list()
                hmvars = list() # Host model to spart dummy args
                for sp_var in spart_args:
                    stdname = sp_var.get_prop_value('standard_name')
                    hvar = host_model.find_variable(stdname)
                    if (hvar is None) and (stdname in CCPP_VAR_LOOP_SUBSTS):
                        lsubst = CCPP_VAR_LOOP_SUBSTS[stdname]
                        hvar = lsubst.find_subst(host_local_vars)
                    # End if
                    if hvar is None:
                        raise CCPPError('No host model variable for {} in {}'.format(stdname, spart.name))
                    # End if
                # End for (loop over part variables)
            # End for (loop of suite parts
            run_stage = stage == 'run'
            # All interfaces need the suite name
            apivars = [__suite_name_var__]
            if run_stage:
                # Only the run phase needs a suite part name
                apivars.append(__suite_part_var__)
            # End if
            apinames = [x.get_prop_value('standard_name') for x in apivars]
            hdvars = host_model.call_list(stage) # Host interface dummy args
            lnames = [x.get_prop_value('local_name') for x in apivars + hdvars]
            api_vlist = ", ".join(lnames)
            cap.write(subhead.format(api_vars=api_vlist,
                                     host_model=host_model.name,
                                     stage=stage), 1)
            # Write out any use statements
            for suite in api.suites:
                mspc = (max_suite_len - len(suite.module))*' '
                for spart in spart_list:
                    cap.write("use {}, {}only: {}".format(suite.module, mspc, spart.name), 2)
                # End for
            # End for
            cap.write("", 1)
            # Write out dummy arguments
            for var in apivars:
                var.write_def(cap, 2, host_model)
            # End for
            for var in hdvars:
                var.write_def(cap, 2, host_model)
            # End for
            for var in host_local_vars.variable_list():
                var.write_def(cap, 2, host_model)
            # End for
            cap.write('', 0)
            # Write out the body clauses
            errmsg_name, errflg_name = api.get_errinfo_names()
            else_str = '\n'
            for suite in api.suites:
                cap.write("{}if (trim(suite_name) == '{}') then".format(else_str, suite.name), 2)
                if stage == 'run':
                    el2_str = ''
                    for spart in spart_list:
                        pname = spart.name[len(suite.name)+1:]
                        cap.write("{}if (trim(suite_part) == '{}') then".format(el2_str, pname), 3)
# XXgoldyXX: v debug only
                        spart_args = spart.call_list.variable_list()
                        hmvars = list() # Host model to spart dummy args
                        for sp_var in spart_args:
                            stdname = sp_var.get_prop_value('standard_name')
                            hvar = host_model.find_variable(stdname)
                            if hvar is None:
                                raise CCPPError('No host model variable for {} in {}'.format(stdname, spart.name))
                            # End if
                            lvars = host_model.loop_vars()
                            lname = host_model.var_call_string(hvar, lvars)
                            hmvars.append(lname)
                        # End for
                        call_str = ', '.join(hmvars)
# XXgoldyXX: ^ debug only
                        cap.write("call {}({})".format(spart.name, call_str), 4)
                        el2_str = 'else '
                    # End for
                    cap.write("else", 3)
                    cap.write("{errmsg} = 'No suite part named '//trim(suite_part)".format(errmsg=errmsg_name), 4)
                    cap.write("{errmsg} = trim({errmsg})//' found in suite {sname}'".format(errmsg=errmsg_name, sname=suite.name), 4)
                    cap.write("{errflg} = 1".format(errflg=errflg_name), 4)
                    cap.write("end if", 3)
                else:
                    call_str = suite.phase_group(stage).call_list.call_string()
                    cap.write("call {}_{}({})".format(suite.name, stage, call_str), 3)
                # End if
                else_str = 'else '
            # End for
            cap.write("else", 2)
            cap.write("{errmsg} = 'No suite named '//trim(suite_name)//' found'".format(errmsg=errmsg_name), 3)
            cap.write("{errflg} = 1".format(errflg=errflg_name), 3)
            cap.write("end if", 2)
            cap.write(subfoot.format(host_model=host_model.name,
                                     stage=stage), 1)
        # End for
        api.write_inspection_routines(cap)
        cap.write(footer.format(module=module_name), 0)
    # End with
    return cap_filename

###############################################################################

if __name__ == "__main__":
    from parse_tools import init_log, set_log_to_null
    logger = init_log('host_registry')
    set_log_to_null(logger)
    # Run doctest
    import doctest
    doctest.testmod()
# No else:
