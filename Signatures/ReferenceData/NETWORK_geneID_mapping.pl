#!usr/bin/perl -w
use strict;
use warnings;

my %gid=();
open(FIN, "<HUGO_HGNC_geneID_mapping_valid.txt") or die "Can't open FIN: $!";
while(<FIN>) {
        chomp;
        my @ar = split (/\t/, $_);
	next if($ar[0] eq "gene");
	$gid{$ar[0]}=$ar[1];
}
close(FIN);

open(FOUT, ">NETWORK.geneID") or die "Can't open FOUT: $!";

open(FIN1, "<NETWORK") or die "Can't open FIN1: $!";
while(<FIN1>) {
        chomp;
        my @ar = split (/\t/, $_);
	if($ar[0] ne "" && $ar[2] ne ""){
		if(exists $gid{$ar[0]} && exists $gid{$ar[2]}){
			if($gid{$ar[0]} ne "NA" && $gid{$ar[2]} ne "NA"){
				print FOUT "$gid{$ar[0]}\t$ar[1]\t$gid{$ar[2]}\n";
			}
		}
	}
}
close(FIN1);
close(FOUT);
