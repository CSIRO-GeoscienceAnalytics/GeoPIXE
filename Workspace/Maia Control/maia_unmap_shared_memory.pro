pro maia_unmap_shared_memory

;	If shared memory is unmapped in this process, then it can't be
;	mapped again with the same name. Hence, only unmap them if the
;	sizes are to change. Then also kill the processes and re-start.

startupp, /error

	shared_memory_unmap, prefix='maia_groups_', n_buffers=4
	shared_memory_unmap, prefix='maia_activity_', n_buffers=2
	shared_memory_unmap, prefix='maia_et_spectra_', n_buffers=1
	shared_memory_unmap, prefix='maia_et_spectraT_', n_buffers=1
	shared_memory_unmap, prefix='maia_et2d_', n_buffers=1
	shared_memory_unmap, prefix='maia_da_', n_buffers=1

return
end

