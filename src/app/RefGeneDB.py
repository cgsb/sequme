#!/usr/bin/python

#
# $Id: RefGeneDB.py,v 1.5 2010-11-11 04:45:21 kirigf01 Exp $
# $Header: /home/kirigf01/cvs/src/pipeline/RefGeneDB.py,v 1.5 2010-11-11 04:45:21 kirigf01 Exp $
# $Date: 2010-11-11 04:45:21 $
#

###
### WARNING: Extremely rough implementation (quick and dirty, to say the least).
###          However, should be (generally) functional.  
###

"""
Table 'refGene' built from:

   http://hgdownload.cse.ucsc.edu/goldenPath/mm9/database/

DATABASE: 'mm9'
 mysql> describe refGene;
+--------------+------------------------------------+------+-----+---------+----------------+
| Field        | Type                               | Null | Key | Default | Extra          |
+--------------+------------------------------------+------+-----+---------+----------------+
| bin          | smallint(5) unsigned               | NO   |     | 0       |                | 
| name         | varchar(255)                       | NO   | MUL |         |                | 
| chrom        | varchar(255)                       | NO   | MUL |         |                | 
| strand       | char(1)                            | NO   |     |         |                | 
| txStart      | int(10) unsigned                   | NO   |     | 0       |                | 
| txEnd        | int(10) unsigned                   | NO   |     | 0       |                | 
| cdsStart     | int(10) unsigned                   | NO   |     | 0       |                | 
| cdsEnd       | int(10) unsigned                   | NO   |     | 0       |                | 
| exonCount    | int(10) unsigned                   | NO   |     | 0       |                | 
| exonStarts   | longblob                           | NO   |     | NULL    |                | 
| exonEnds     | longblob                           | NO   |     | NULL    |                | 
| id           | int(10) unsigned                   | NO   | PRI | NULL    | auto_increment | 
| name2        | varchar(255)                       | NO   | MUL |         |                | 
| cdsStartStat | enum('none','unk','incmpl','cmpl') | NO   |     | none    |                | 
| cdsEndStat   | enum('none','unk','incmpl','cmpl') | NO   |     | none    |                | 
| exonFrames   | longblob                           | NO   |     | NULL    |                | 
+--------------+------------------------------------+------+-----+---------+----------------+
16 rows in set (0.00 sec)

-----

Genes that have cdsStart equal to cdsEnd are non-coding genes.
Please note the display description page:
  http://genome.ucsc.edu/cgi-bin/hgTrackUi?db=hg18&c=chr1&g=knownGene

From:
  https://lists.soe.ucsc.edu/pipermail/genome/2007-July/014180.html

  ' .. for genes located on the negative strand, we still enter the 
end points in the underlying database table in the following order:

txStart --> cdsStart --> cdsEnd --> txEnd

This way, they are similar to the entries for genes on the positive 
strand.  We store them in the database table this way because it 
simplifies calculations when we are preparing to display them.

So, although it may indicate in the table that txStart is at position 
123456, that is actually just the location for the *left* side of the 
gene in the display.  In the browser, read from left to right for genes 
on the positive strand; from right to left for genes on the negative strand.'


See below page for more details on calling mysql from python:
   http://www.kitebird.com/articles/pydbapi.html


"""

__author__ = "Francis Kirigin francis.kirigin@med.nyu.edu"
__version__ = "$Revision: 1.5 $"
__date__ = "$Date: 2010-11-11 04:45:21 $"
__copyright__ = "Copyright (c) 2010 Francis Kirigin, Dan Littman Laboratory"
__license__ = "Python"


#
# Flags
#

do_debug = False


import MySQLdb

#
# Define an enumuration-like type for classifying
# chromosomal location mapping
#

EXON_POS, INTRON_POS, INTERGENE_POS = range(3)
classify_chr = ['Exon', 'Intron', 'Intergenic']
genic_class = ['Exon', 'Intron', 'Inter']

#
# Connect to mm9 database, lookup gene by name or name2 
#

# NOTE: Non-coding genes can be determined by gene record in two ways:
#        1) cdsStartStat = "unk"
#        2) cdsStart = cdsEnd 
#
# Current copy of the database has 1477 such entries
#

class RefGeneDB :
     "Accesses refGene database from UCSC"
     def __init__ (self) :
          self.cursor = ""
          self.conn = ""

     def connect(self) :
          self.conn = MySQLdb.connect(host = "localhost",
                                      user = "root",
                                      passwd = "aids",
                                      db = "mm9")
          self.cursor = self.conn.cursor()

     def get_gene_by_name(self, name) :
          """
          Given a gene name, retrieve full refGene record(s), if exists.
          NOTE: For now just returns boolean on existance.
          """
          
          print "RefGeneDB::get_gene_by_name: name=%s" % name

          query = "select * from refGene where name = \"" + name + "\";"

          if do_debug :
               print "RefGeneDB::get_gene_by_names: QUERY: '%s'" % query

          self.cursor.execute(query)
          row = self.cursor.fetchone()

          if row == None :
               print "NO ROW FOUND"

          else :
               print "FOUND ROW, total fields: %d" % len(row)
               print "ROW: ", row[0], " [1]=", row[1], " [2]=", row[2]

          return "FOUND"

     def get_gene_by_name2(self, name2) :
          """
          Given a gene name2, retrieve full refGene record(s), if exists.
          NOTE: For now just returns boolean on existance.
          """
          
          query = "select * from refGene where name2 = \"" + name2 + "\";"

          if do_debug :
               print "RefGeneDB::get_gene_by_names: name2=%s, query='%s'" % (name2, query)

          self.cursor.execute(query)

          # To retrieve all the entries that match the query, use fetchall() API
          #  http://docs.python.org/library/sqlite3.html#sqlite3.Cursor.fetchall

          rows = self.cursor.fetchall()
          row = rows[0]

          if row == None :
               print "NO ROW FOUND"

          else :
               print "FOUND %d ROWS, total fields: %d" % (len(rows), len(row))
               print "ROW: ", row[0], " [1]=", row[1], " [2]=", row[2]
               print "ROW: ", row

          return "FOUND"


     def get_gene_by_names(self, names) :
          """Given a list of gene names, retrieve full refGene record(s), for all that exist"""
          
          print "RefGeneDB::get_gene_by_name: total names: %d" % len(names)

          do_dbg = False
          chr_tuples = dict()

          gene_hash = {}

          for nm in names.iteritems() :
               if do_dbg :
                    print "RefGeneDB::get_gene_by_names: name=%s" % nm

               query = "select * from refGene where name = \"" + nm + "\";"

               if do_dbg :
                    print "RefGeneDB::get_gene_by_names: QUERY: '%s'" % query

               self.cursor.execute(query)
               row = self.cursor.fetchone()

               gene_hash[nm] = row

          return gene_hash


     # Given an chromosomal lower and upper bound, find all genes
     # whose txStart in within the range

     # If gene found, returns:
     # strand, name2, is_coding, nth, total_exon, dist_TSS
     #
     #  strand:   + or -
     #  gene:     Gene name with nearest TSS (TO DO)
     #  is_exon:  True for exon, False for intron
     #  nth:      Number of exon or intron or NA
     #  dist_TSS: Distance from Txn Start Site, negative

     def find_in_range(self, chrom, lower, upper, summit) :
          """Given a region summit, determine if existing
          gene overlaps within the given range"""
          
          do_debug = True
          found = False

          ###
          ### FIXME: Should only perform three queries, checking for genes where:
          ###         1) TSS is between upper and lower kb range
          ###         2) TES is between upper and lower kb range
          ###         3) Summit is between TSS and TES
          ###            - Check summit to find genes with TSS and TES outside interval
          ###

          # Previously:
          #  To extract all relative rows within interval:
          #   - First query exact summit, if lands on any gene. Next
          #     query if TSS is within the lower and higher 10kb bound;
          #     lastly, query if TES is within the lower and higher 10kb bound.
          #   - Combine all 3 row results
          #   - Resolve all returned rows by getting unique by name2,
          #     selecting the entry with the closest TSS to summit

          all_rows = []
          
          # First find all genes that match a TSS within upper and lower range

          query_pfx = "select * from refGene where chrom = \"" + chrom + "\" and "
          query = query_pfx + " txStart > " + str(lower) + " and txStart < " + str(upper) + ";"
          self.cursor.execute(query)
          summit_rows = self.cursor.fetchall()
          n_rows = self.cursor.rowcount
          
          if do_debug :
               print "RefGeneDB::find_in_range: SUMMIT QUERY 1: '%s'" % query
               print "FOUND %d FOR SUMMIT QUERY" % n_rows

          if n_rows > 0 :
               all_rows.extend(summit_rows)
               
          # Next find all genes that match a TES within upper and lower range

          query = query_pfx + " txEnd > " + str(lower) + " and txEnd < " + str(upper) + ";"
          self.cursor.execute(query)
          summit_rows = self.cursor.fetchall()
          n_rows = self.cursor.rowcount

          if do_debug :
               print "RefGeneDB::find_in_range: SUMMIT QUERY 2: '%s'" % query
               print "FOUND %d FOR SUMMIT QUERY" % n_rows
               
          if n_rows > 0 :
               all_rows.extend(summit_rows)

          # Finally, to handle genes bigger than the 2*interval, find all genes
          # larger than the interval, so summit is between the start and end

          query = query_pfx + " txStart < " + str(summit) + " and txEnd > " + str(summit) + ";"
          self.cursor.execute(query)
          summit_rows = self.cursor.fetchall()
          n_rows = self.cursor.rowcount

          if do_debug :
               print "RefGeneDB::find_in_range: SUMMIT QUERY 3: '%s'" % query
               print "FOUND %d FOR SUMMIT QUERY" % n_rows

          if n_rows > 0 :
               all_rows.extend(summit_rows)

          uniq_rows = resolve_dups(all_rows, summit)
          all_genes = {}
          
          print "Number of unique rows resolved: %d" % len(uniq_rows)

          for row in uniq_rows:
               print "find_in_range: Unique row: %s, %s" % (row[0], row[1])
               
               strand, gene, is_coding, nth, n_exon, d_TSS, d_TES, chr_class, \
                       gene_len = lookup_chr(chrom, summit, row, do_debug)
               gene_info = [strand, gene, nth, is_coding, d_TSS, d_TES, \
                            genic_class[chr_class], gene_len]
               all_genes[gene] = gene_info               

               print "GENE INFO", gene_info

          found = False

          if len(uniq_rows) > 0 :
               found = True

          return found, all_genes


     def get_by_position(self, chrom, pos) :
          """Given a single chromosomal position, determine if corresponds 
          to an intronic, exonic, or intergenic location"""
          
          do_dbg = True 
          query = "select * from refGene where chrom = \"" + chrom + \
                  "\" and txStart < " + pos + " and txEnd > " + pos + ";"

          if do_dbg :
               print "RefGeneDB::get_by_position: QUERY: '%s'" % query

          self.cursor.execute(query)
          row = self.cursor.fetchone()
          strand, gene, chr_class, is_coding = lookup_chr(chrom, pos, row, do_debug)
               
          # chr, start, strand, gene, class
          classified = [chrom, pos, strand, gene, genic_class[chr_class], is_coding]
                    
          if do_dbg :
               print "RefGeneDB::get_by_position: ", classified

          return classified


     def find_gene_in_summit(self, chrom, summit, real_summit) :
          """Given a region summit, determine if existing
          gene overlaps within the given range"""
          
          do_dbg = True
          found = False

          lower = int(summit) - 10000
          upper = int(summit) + 10000

          query = "select * from refGene where chrom = \"" + chrom + \
                  "\" and txStart > " + str(lower) + " and txStart < " + \
                  str(summit) + ";"

          if do_dbg :
               print "RefGeneDB::classify: QUERY: '%s'" % query

          self.cursor.execute(query)

          rows = self.cursor.fetchall()
          n_rows = self.cursor.rowcount
          
          if n_rows < 1 :
               row = []
               if do_debug :
                    print "NO ROW FOUND"
               return found, row
                    
          print "Number of rows returned: %d" % n_rows
          all_genes = {}
          
          for row in rows:
               print "%s, %s" % (row[0], row[1])
               strand, gene, is_coding, nth, n_exon, d_TSS, d_TES, flat, gene_len = \
                       lookup_chr(chrom, real_summit, row, do_debug)
               gene_info = [strand, gene, nth, is_coding, d_TSS, d_TES, flat, gene_len]
               print "GENEINFO", gene_info               
               all_genes[gene] = gene_info

          # If gene found, returns:
          #  strand, name2, is_coding, nth, n_exon, dist_TSS
          #
          #  strand:   + or -
          #  gene:     Gene name with nearest TSS (TO DO)
          #  is_exon:  True for exon, False for intron
          #  nth:      Number of exon or intron or NA
          #  dist_TSS: Distance from Txn Start Site, negative

          return True, all_genes #  gene_info


     def close(self) :
          self.cursor.close()
          self.conn.close()


def resolve_dups(rows, summit) :

     """
     Resolves all duplicate entries, such that only a single gene
     can match a set of rows returned from query.
     Eliminates multiplicity due to isoforms, selecting the candidate
     with the closest TSS to the summit of interest.
     Assumes field 'name2' is unique-able element.
     """

     genes = {}
     resolved = []

     for row in rows :
          name2 = row[12]
          txStart = int(row[4])
          txEnd = int(row[5])
          gene_len = txEnd - txStart
          strand = row[3]
     
          anti_sense = (strand == "-")
          tss = txStart

          if anti_sense :
               tss = txEnd

          d_tss = abs(tss - summit)

          if not genes.has_key(name2) :
               genes[name2] = {}
               genes[name2]["len"] = gene_len
               genes[name2]["row"] = row
               genes[name2]["d_tss"] = d_tss
               
               print "resolve_dups: Added new gene entry: ", genes[name2]
               
               continue

          if d_tss < genes[name2]["d_tss"] : 
               print "resolve_dups: Found closer tss: ", genes[name2], " d_tss=%d" % d_tss

               genes[name2]["len"] = gene_len
               genes[name2]["row"] = row
               genes[name2]["d_tss"] = d_tss

          else :
               print "resolve_dups: Skipping isoform with larger tss: %d vs %d" % \
                     (genes[name2]["d_tss"], d_tss)

     unames = genes.keys()
     clean = []

     for name in unames :
          clean.append(genes[name]["row"])

     print "resolve_dups: Found %d unique, %d original" % (len(clean), len(rows))

     return clean


##
# Given a chromosome and chromosomal location, queries the
# 'knownGene' database to determine if the coordinates 
# correspond to known genes.
#
# Determine if location lands on an exon.  As exon start
# and end positions are ordered in increasing positions
# on the gene, check exon ends first to find the lowest 
# ending that is greater than the given sequence location.
#
# If the corresponing exon start is less than the sequence
# location, the position lands on an exonic region.
# Otherwise, the sequence corresponds to an intron region.
#
###
### NOTE: UNDER CONSTRUCTION
###

# @chr: Chromosome to query
# @loc: Location on given chromosome
# @db_cursor: Open database cursor
# @do_debug: Set debugging mode  
#
# return:
# @strand: Strand of gene found: '+' or '-', 'NA' if no gene found
# @gene: Name of gene found, 'NONE' if no gene found
# @chr_class: Class of chromosome
#

def lookup_chr(chr, loc, row, do_debug) :
     do_debug = True

     if do_debug :
          print "ENTERED find_by_location: chr=%s loc=%s" % (chr, loc)

     # As default, assume the position is intergenic
     chr_class = INTERGENE_POS
     gene = "NA"
     strand = "NA"
     is_coding = False
     
     if row == None :
          if do_debug :
               print "NO ROW FOUND"
          return strand, gene, chr_class, is_coding
     
     if do_debug :
          print "POSITION LOCATED ON GENE: ", row

     gene = row[1]
     name2 = row[12]
     strand = row[3]
     txStart = int(row[4])
     txEnd = int(row[5])
     
     n_exon = row[8]
     exon_starts = row[9].split(",")
     exon_ends = row[10].split(",")
     location = int(loc)
     nth_exon = 0
     is_intron = False

     if location > txStart and location < txEnd :
          chr_class = INTRON_POS

     first_exon_start = exon_starts[0]

     tx_equal_first_exon = False

     flat = "|".join(map(str, row))
     print "gene: %s : flat: ", gene, flat

     if n_exon > 0 :
          tx_equal_first_exon = int(txStart) == int(first_exon_start)
    
     for i in range(0, n_exon) :
        exon_end = int(exon_ends[i])

        if location < exon_end :
            exon_start = int(exon_starts[i])
        
            if location > exon_start :
                 if do_debug :
                      print "EXON: start=%d <= loc=%d < end=%d" % \
                            (exon_start, location, exon_end)
                 chr_class = EXON_POS

            else :
                 is_intron = True

                 if do_debug : 
                      print "INTRON LOCATION: start=%s > loc=%d < end=%s" % \
                            (exon_start, location, exon_end)
            nth_exon = i 
            break

     nth = nth_exon
     TSS = txStart

     is_sense = True

     if strand == "-" :
          is_sense = False
          nth = n_exon - nth_exon + 1
          TSS = exon_ends[(n_exon - 1)]

     first_exon_size = int(exon_ends[0]) - int(exon_starts[0])

     cd_start = row[5]
     cd_end = row[6]

     if cd_start != cd_end : 
          is_coding = True

     first_intron_size = 0

     print "n_exon=%d" % n_exon

     if n_exon > 1 and len(exon_starts) > 1 :
          first_intron_size = int(exon_starts[1]) - int(exon_ends[0]) 

     print "name=%s name2=%s" % (gene, name2)

     summit = int(loc)
     dist_TSS = int(TSS) - summit

     if strand == "-" :
          dist_TSS = -dist_TSS
     name_mod = name2 + "," + str(nth) + "," + str(n_exon) + "," + str(dist_TSS)

     print "MODIFIED NAME: %s" % name_mod

     d_TSS = 0
     d_TES = 0

     # For the '+' strand:
     #  - d_TSS = summit - TSS (negative value means peak is upstream of TSS,
     #    positive value means downstream)
     #  - d_TES = summit - TES (the same as above, negative means peak is
     #    upstream, positive means peak is downstream)
     #
     # For the '-' strand:
     #  - d_TSS = TSS - summit (negative value still means peak is upstream of TSS)
     #  - d_TES = TES - summit (negative value still means peak is upstream of TSS)
     # 

     if is_sense :
          d_TSS = loc - txStart
          d_TES = loc - txEnd
     else :          
          d_TSS = txEnd - loc
          d_TES = txStart - loc

     #  strand:   + or -
     #  gene:     Gene name with nearest TSS (TO DO)
     #  is_exon:  True for exon, False for intron
     #  nth:      Number of exon or intron or NA
     #  dist_TSS: Distance from Txn Start Site, negative

     gene_len = txEnd - txStart

     return strand, name2, is_coding, nth, n_exon, d_TSS, d_TES, chr_class, gene_len

