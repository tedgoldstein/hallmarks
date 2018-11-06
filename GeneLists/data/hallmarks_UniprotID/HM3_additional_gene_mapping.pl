#!usr/bin/perl -w
use strict;
use warnings;

my %gid=();
open(FIN, "<HUGO_HGNC_geneID_mapping_all.txt") or die "Can't open FIN: $!";
while(<FIN>) {
        chomp;
        my @ar = split (/\t/, $_);
	next if($ar[0] eq "HGNC ID");
	next if($ar[1] =~ /.*withdrawn.*/);
	next if($ar[2] =~ /.*withdrawn.*/);
	if(@ar > 5){
		my $id;
		if($ar[3] ne ""  && $ar[5] eq ""){
			$id=$ar[3];
			next;
		}elsif($ar[5] ne ""  && $ar[3] eq ""){
			$id=$ar[5];
			next;
		}elsif($ar[3] ne ""  && $ar[5] ne "" && $ar[3] eq $ar[5]){
			$id=$ar[3];
		}else{
			print "$_\n";
		}
		$gid{$ar[1]}=$id;
	}
}
close(FIN);

open(FOUT, ">annoAdd_268_genes_toHM3.geneID.mapping") or die "Can't open FOUT: $!";

open(FIN1, "<annoAdd_268_genes_toHM3") or die "Can't open FIN1: $!";
while(<FIN1>) {
        chomp;
        my @ar = split (/\t/, $_);
	if(exists $gid{$ar[0]}){
		print FOUT "$ar[0]\t$gid{$ar[0]}\n";
	}else{
		print FOUT "$ar[0]\t\n";
	}
}
close(FIN1);
close(FOUT);
