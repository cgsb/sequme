#!/usr/bin/python

#
# Small script that takes two arguments:
#  ARG[1]: One side of mates from paired-end sequence data. FASTQ format
#          (ie. left mates or right mates)
#  ARG[2]: List of FASTQ ids from other side of mates that are considered
#          to be invalid reads, so must be removed from corresponding
#          FASTQ sequence data.
#
# $Id: correct_pairs.py,v 1.0 $
#

import sys

def sort_dict_values(src_dict):
    keys = src_dict.keys()
    keys.sort()
    return [src_dict[key] for key in keys]

def append_lines(dest_file, lines) :
    out_handle = open(dest_file, "a")
    out_handle.writelines(lines)
    out_handle.close()


# $Id: $

#
# Parse command line
#
"""
from optparse import OptionParser

usage = "\n\nNAME: \n\t correct_pairs.py - edits and manipulates sequence datadata\n\n" + \
    "SYNOPSIS: \n\t correct_pairs.py [-f, --filename FASTA]\n" + \
    "\t correct_pairs.py [-h, --help]\n\n" + \
    "DESCRIPTION: \n\t Allows for corrections of two mate pairs of sequence data, such that" + \
    " mated pairs between the left and right sequence data sets are collineated, ie. an intersection has been performed.\n" + \
    "Actions supported for editing include: add, update, delete, extract.\n"

parser = OptionParser(usage=usage)
parser.add_option("-f", "--file", dest="filename",
                  help="TO DO")
(options, args) = parser.parse_args()

"""

#
# Expect two arguments, input file to correct, and listing of
# unmatched read id's, prefixed to trim paired end mate field
#

if len(sys.argv) < 3 :
    print "usage: python %s [FASTQ file] [List of bad pairs]" % sys.argv[0]
    sys.exit(1)
    
mates_fq_fl = sys.argv[1]
print "Examining one side of mates from paired-end sequence data: %s" % mates_fq_fl

#
# Create output files
#

import site
from datetime import datetime, date, time
from time import gmtime, strftime

exec_tag = datetime.now().strftime("%m%d-%H%M")
fq_flds = mates_fq_fl.split(".")
out_file = fq_flds[0] + "-pair-" + exec_tag + ".fq"

lines_ofile = fq_flds[0] + "-pairln-" + exec_tag + ".fq"
omit_ofile = fq_flds[0] + "-omit-" + exec_tag + ".txt"
refd_ofile = fq_flds[0] + "-refd-" + exec_tag + ".txt"

print "OUT FILE: %s" % out_file
print "PRE-READ TIMESTAMP:", exec_tag, " FASTQ FILE: ", mates_fq_fl

#
# Load FASTQ file to correct
#

handle = open(mates_fq_fl, "r")
seq_lines = handle.readlines()
handle.close()

read_done = datetime.now().strftime("%m%d-%H%M")
print "POST-READ TIMESTAMP:", read_done, " (PRE: ", exec_tag, ") FASTQ FILE: ", mates_fq_fl


#
# Load bad mate id's
#

bad_mates_fl = sys.argv[2]
print "Examining list of bad mates ids: %s" % bad_mates_fl

from Bio import SeqIO
 
handle = open(bad_mates_fl, "r")
lines = handle.readlines()  
handle.close()

bad_pfx = dict()

for line in lines :
    pfx = line.rstrip()
    bad_pfx[pfx] = pfx

#
# Process paired end data, eliminating entries whose
# read id exists in the orphaned list
#

nline = 0
read_id = ""
read_seq = ""
read_entry = ""

dest_lines = []
omit_pfxs = []
refd_pfxs = []

is_bad = False
passed = dict()
nth_pass = 0
pfx = ""
 
for line in seq_lines :
    entry = line.rstrip()

    if nline == 0 or entry[0] == "@" :
        nline = 1
        read_id = entry
        id_len = len(read_id)
        pfx_len = id_len - 4
        pfx = read_id[:pfx_len]  
#        print "FASTQ READ PREFIX: %s, ID: %s" % (pfx, read_id)

        if bad_pfx.has_key(pfx) :
#            print "FOUND BAD PREFIX: ", entry
            is_bad = True
        else :
            read_entry = line
            seq_line = entry + " "

    elif (nline == 1 or nline == 2) :
        if not is_bad :
            read_entry += line
            seq_line += entry + " "
        nline = nline + 1

    elif nline == 3 :
        nline = 0
        nth_pass = nth_pass + 1
        pfxd = pfx + "\n"
#        print "PREFIX:", pfxd

        if is_bad :
            is_bad = False
            omit_pfxs.append(pfxd)

        else :
            seq_line += line
            dest_lines.append(seq_line)
            refd_pfxs.append(pfxd)   #            passed[entry] = read_entry + line  

        if nth_pass >= 1000000 :
            nth_pass = 0
            
            print "OMITTING IN THIS BLOCK", omit_pfxs

            append_lines(lines_ofile, dest_lines)
            dest_lines = []
            append_lines(omit_ofile, omit_pfxs)
            omit_pfxs = []
            append_lines(refd_ofile, refd_pfxs)
            refd_pfxs = []

            sys.stdout.flush() #            passed[entry] = read_entry + line  #            passed.append(read_entry)

if nth_pass > 1 :
    append_lines(lines_ofile, dest_lines)
    append_lines(omit_ofile, omit_pfxs)
    append_lines(refd_ofile, refd_pfxs)

#cleared = passed.values()
#append_lines(out_file, cleared)

#frmtd = sort_dict_values(passed)
#vals = frmtd  #.values()

#out_file = out_file + ".srt"
#out_handle = open(out_file, "w")
#out_handle.writelines(vals)
#out_handle.close()


