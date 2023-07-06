#' Make input files for DeepDive where replicates are saved in separate files
#'
#' 'write_dd_files()' uses the data to make input files for use in DeepDive
#' @param dat Column-wise occurrence data with taxon ID, Area, MinAge, MaxAge,
#' Locality columns.
#' @param r The number of age assignment replicates
#' @param age_m The age assignment method
#' @param taxon_level The taxonomic level the data are identified to
#' @param bins The time bins specified for the data set
#' @returns Saves a csv file for each age replicate with Type column indicating
#' whether data are locality counts or the binned occurrence data per region.
#' @examples
#' write_dd_files(dat, r=replicate, taxon_level="Species", bins=bins,
#' age_m=age_method)
#' @export
write_dd_files <- function(dat, r=replicate, age_m="median", taxon_level, bins){
  sampled_dat <- ages(dat, method=age_m)  # Get ages and append
  area_tables <- split(sampled_dat, f = sampled_dat$Area)  # Split data by area

  # Get species or genera level data
  occs <- taxa_time_per_area(sampled_dat, area_tables, bins=bins,
                             taxonomic_level=taxon_level)

  # Get locality data
  locs <- generate_locality_dataset(sampled_dat, bins=bins)

  deepdive_input <- rbind(locs, occs)
  write.csv(deepdive_input, paste0(name, "empirical_data/", r, "deepdive_input.csv"), row.names=FALSE)

  # generate_occurrence_dataset(sampled_dat, area_tables, bins=bins, res, name)
  # occs_per_loc <- occs/(locs+1) # Occurrences per locality through time for each area
}


#' Generate DeepDive input in one file
#'
#' 'write_dd_files2()' makes a single input csv file for use in DeepDive.
#' @param dat Data frame of occurrences with taxon ID, Area, MinAge, MaxAge and
#' Locality columns.
#' @param r The number of age assignment replicates
#' @param age_m The age assignment method
#' @param taxon_level The taxonomic level the data are identified to
#' @param bins Time bins used in the analysis (should reflect the empirical data
#' and be identical to the bins used in any corresponding DeepDive simulations
#' and training)
#' @returns Here each age replicate is indicated in the 'R' column and can be
#' set using the r setting. Locality and occurrence data by time bin are
#' generated alongside the replicate number, the data type (either locs or occs)
#' and the region ID.
#' @examples
#' write_dd_files2(dat, r=replicate, taxon_level="Genus", bins=bins,
#' age_m="median")
#' @export
write_dd_files2 <- function(dat, r=replicate, age_m="median", taxon_level, bins){
  deepdive_input <- data.frame()
  for(rep in 1:r){
    sampled_dat <- ages(dat, method=age_m) # Get ages and append
    area_tables <- split(sampled_dat, f = sampled_dat$Area)  # Split data by area

    # Get species or genera level data
    occs <- taxa_time_per_area(sampled_dat, area_tables, bins=bins,
                               taxon_level=taxon_level)
    cnames <- c(colnames(occs))

    # Get locality data
    locs <- generate_locality_dataset(sampled_dat, bins=bins)

    # Get time bin data
    tbins <- data.frame(cbind(c("bin_mid", "bin_dur"), NA,
                              rbind(bins$midpoint, bins$start-bins$end)))
    names(tbins) <- cnames

    dd_input <- rbind(locs, occs)
    dd_input <- cbind(R=rep, dd_input)
    deepdive_input <- rbind(deepdive_input, dd_input)
  }
  write.csv(deepdive_input, paste0(name, "empirical_data/deepdive_input.csv"),
            row.names=FALSE)
}


#' Prep DeepDive Input
#'
#' 'prep_dd_input()' makes a single input file for use in DeepDive in one line.
#' @param dat Data frame of occurrences with taxon ID, Area, MinAge, MaxAge and
#' Locality columns.
#' @param bin_type Determines how bins will be made (equal bins, epochs, stages)
#' @param begin Starting age of the oldest time bin
#' @param finish Ending age of the youngest time bin
#' @param n_bins Number of bins (used only when equal_bins are specified)
#' @param remove_bins Specify named time bins to be removed (quote directly or
#' use a list object)
#' @param use_q Merge time bins in the Quaternary together
#' @param merge_holo Merge time bins in the Holocene together
#' @param merge_holo_ple Merge time bins in the Upper Pleistocene and Holocene
#' @param r The number of age assignment replicates
#' @param age_m The age assignment method
#' @param taxon_level The taxonomic level the data are identified to
#' @param age_range_threshold An age range cut-off between high and low res
#' occurrences (default=NA)
#' @returns Here each age replicate is indicated in the 'R' column and can be
#' set using the r setting. Locality and occurrence data by time bin are
#' generated alongside the replicate number, the data type (either locs or occs)
#' and the region ID.
#' @examples
#' prep_dd_input(dat=dat_res, scale="stages", begin=, finish=, r=replicates, age_m = age_method,
#' taxon_level=taxonomic_level, all_lr=T)
#' @export
prep_dd_input <- function(dat=dat, bin_type=bins_scale, begin=begin_bins,
                          finish=end_bins, n_bins=10, custom_bins=NULL,
                          lr_hr_bins="", remove_bins=NULL, use_q=FALSE,
                          merge_holo=FALSE, merge_holo_ple=FALSE, r=replicate,
                          age_m=age_method, taxon_level=taxonomic_level,
                          age_range_threshold=NA, all_lr=F, write_file=F){

  # Make time bins
  bins <- time_bins(bin_type=bin_type, begin=begin, finish=finish,
                    n_bins=n_bins, custom_bins=custom_bins,
                    lr_hr_bins=lr_hr_bins, remove_bins=remove_bins, use_q=use_q,
                    merge_holo=merge_holo, merge_holo_ple=merge_holo_ple)


  # Assign occurrences as high or low resolution
  dat_res <- assign_res(dat, age_range_threshold=age_range_threshold,
                        all_lr=all_lr)


  deepdive_input <- data.frame()
  for(rep in 1:r){

    sampled_dat <- ages(dat, method=age_m) # Get ages and append
    area_tables <- split(sampled_dat, f = sampled_dat$Area)  # Split data by area

    # Get species or genera level data
    occs <- taxa_time_per_area(sampled_dat, area_tables, bins=bins,
                               taxon_level=taxon_level)
    cnames <- c(colnames(occs))

    # Get locality data
    locs <- generate_locality_dataset(sampled_dat, bins=bins)

    # Get time bin data
    tbins <- data.frame(cbind(c("bin_mid", "bin_dur"), NA,
                              rbind(bins$midpoint, bins$start-bins$end)))
    names(tbins) <- cnames

    dd_input <- rbind(tbins, locs, occs)
    dd_input <- cbind(R=rep, dd_input)
    deepdive_input <- rbind(deepdive_input, dd_input)
  }

  # Get DeepDive input file
  if(write_file ==T){
    write.csv(deepdive_input, paste0(name, "empirical_data/deepdive_input.csv"),
              row.names=FALSE)
  }
  return(deepdive_input)
}
