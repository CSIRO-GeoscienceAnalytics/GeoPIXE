pro cleanup_sems

n_buffers = 16
n_buffers2 = 16

prefix_spectra = 'blog_client_spectra_'
sem_name_spectra = allocate_sems( n_buffers, prefix=prefix_spectra, error=error)
;if error ne 0 then goto, bad_sem

prefix_da = 'blog_client_da_'
sem_name_da = allocate_sems( n_buffers2, prefix=prefix_da, error=error)
;if error ne 0 then goto, bad_sem

release_sems, sem_name_spectra
release_sems, sem_name_da
return
end
