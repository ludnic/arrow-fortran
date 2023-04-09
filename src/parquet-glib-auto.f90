! File automatically generated by cfwrapper.py (gtk-fortran) in Fedora Rawhide
! 2023-04-07

module parquet_glib
use, intrinsic :: iso_c_binding
implicit none
interface

!--------------------------------------------------
! /usr/include/parquet-glib/metadata.hpp
!--------------------------------------------------
!--------------------------------------------------
! /usr/include/parquet-glib/parquet-glib.h
!--------------------------------------------------
!--------------------------------------------------
! /usr/include/parquet-glib/statistics.hpp
!--------------------------------------------------
!--------------------------------------------------
! /usr/include/parquet-glib/metadata.h
!--------------------------------------------------
! 
!gboolean gparquet_column_chunk_metadata_equal( GParquetColumnChunkMetadata *metadata, GParquetColumnChunkMetadata *other_metadata);
function gparquet_column_chunk_metadata_equal(metadata, other_metadata) bind(c)
  import :: c_int, c_ptr
  implicit none
  integer(c_int) :: gparquet_column_chunk_metadata_equal
  type(c_ptr), value :: metadata
  type(c_ptr), value :: other_metadata
end function

! 
!gint64 gparquet_column_chunk_metadata_get_total_size( GParquetColumnChunkMetadata *metadata);
function gparquet_column_chunk_metadata_get_total_size(metadata) bind(c)
  import :: c_int64_t, c_ptr
  implicit none
  integer(c_int64_t) :: gparquet_column_chunk_metadata_get_total_size
  type(c_ptr), value :: metadata
end function

! 
!gint64 gparquet_column_chunk_metadata_get_total_compressed_size( GParquetColumnChunkMetadata *metadata);
function gparquet_column_chunk_metadata_get_total_compressed_size(metadata)&
& bind(c)
  import :: c_int64_t, c_ptr
  implicit none
  integer(c_int64_t) :: gparquet_column_chunk_metadata_get_total_compressed_size
  type(c_ptr), value :: metadata
end function

! 
!gint64 gparquet_column_chunk_metadata_get_file_offset( GParquetColumnChunkMetadata *metadata);
function gparquet_column_chunk_metadata_get_file_offset(metadata) bind(c)
  import :: c_int64_t, c_ptr
  implicit none
  integer(c_int64_t) :: gparquet_column_chunk_metadata_get_file_offset
  type(c_ptr), value :: metadata
end function

! 
!gboolean gparquet_column_chunk_metadata_can_decompress( GParquetColumnChunkMetadata *metadata);
function gparquet_column_chunk_metadata_can_decompress(metadata) bind(c)
  import :: c_int, c_ptr
  implicit none
  integer(c_int) :: gparquet_column_chunk_metadata_can_decompress
  type(c_ptr), value :: metadata
end function

! 
!GParquetStatistics * gparquet_column_chunk_metadata_get_statistics( GParquetColumnChunkMetadata *metadata);
function gparquet_column_chunk_metadata_get_statistics(metadata) bind(c)
  import :: c_ptr
  implicit none
  type(c_ptr) :: gparquet_column_chunk_metadata_get_statistics
  type(c_ptr), value :: metadata
end function

! 
!gboolean gparquet_row_group_metadata_equal(GParquetRowGroupMetadata *metadata, GParquetRowGroupMetadata *other_metadata);
function gparquet_row_group_metadata_equal(metadata, other_metadata) bind(c)
  import :: c_int, c_ptr
  implicit none
  integer(c_int) :: gparquet_row_group_metadata_equal
  type(c_ptr), value :: metadata
  type(c_ptr), value :: other_metadata
end function

! 
!gint gparquet_row_group_metadata_get_n_columns(GParquetRowGroupMetadata *metadata);
function gparquet_row_group_metadata_get_n_columns(metadata) bind(c)
  import :: c_int, c_ptr
  implicit none
  integer(c_int) :: gparquet_row_group_metadata_get_n_columns
  type(c_ptr), value :: metadata
end function

! 
!GParquetColumnChunkMetadata * gparquet_row_group_metadata_get_column_chunk(GParquetRowGroupMetadata *metadata, gint index, GError **error);
function gparquet_row_group_metadata_get_column_chunk(metadata, index, error)&
& bind(c)
  import :: c_ptr, c_int
  implicit none
  type(c_ptr) :: gparquet_row_group_metadata_get_column_chunk
  type(c_ptr), value :: metadata
  integer(c_int), value :: index
  type(c_ptr), value :: error
end function

! 
!gint64 gparquet_row_group_metadata_get_n_rows(GParquetRowGroupMetadata *metadata);
function gparquet_row_group_metadata_get_n_rows(metadata) bind(c)
  import :: c_int64_t, c_ptr
  implicit none
  integer(c_int64_t) :: gparquet_row_group_metadata_get_n_rows
  type(c_ptr), value :: metadata
end function

! 
!gint64 gparquet_row_group_metadata_get_total_size( GParquetRowGroupMetadata *metadata);
function gparquet_row_group_metadata_get_total_size(metadata) bind(c)
  import :: c_int64_t, c_ptr
  implicit none
  integer(c_int64_t) :: gparquet_row_group_metadata_get_total_size
  type(c_ptr), value :: metadata
end function

! 
!gint64 gparquet_row_group_metadata_get_total_compressed_size( GParquetRowGroupMetadata *metadata);
function gparquet_row_group_metadata_get_total_compressed_size(metadata)&
& bind(c)
  import :: c_int64_t, c_ptr
  implicit none
  integer(c_int64_t) :: gparquet_row_group_metadata_get_total_compressed_size
  type(c_ptr), value :: metadata
end function

! 
!gint64 gparquet_row_group_metadata_get_file_offset( GParquetRowGroupMetadata *metadata);
function gparquet_row_group_metadata_get_file_offset(metadata) bind(c)
  import :: c_int64_t, c_ptr
  implicit none
  integer(c_int64_t) :: gparquet_row_group_metadata_get_file_offset
  type(c_ptr), value :: metadata
end function

! 
!gboolean gparquet_row_group_metadata_can_decompress(GParquetRowGroupMetadata *metadata);
function gparquet_row_group_metadata_can_decompress(metadata) bind(c)
  import :: c_int, c_ptr
  implicit none
  integer(c_int) :: gparquet_row_group_metadata_can_decompress
  type(c_ptr), value :: metadata
end function

! 
!gboolean gparquet_file_metadata_equal(GParquetFileMetadata *metadata, GParquetFileMetadata *other_metadata);
function gparquet_file_metadata_equal(metadata, other_metadata) bind(c)
  import :: c_int, c_ptr
  implicit none
  integer(c_int) :: gparquet_file_metadata_equal
  type(c_ptr), value :: metadata
  type(c_ptr), value :: other_metadata
end function

! 
!gint gparquet_file_metadata_get_n_columns(GParquetFileMetadata *metadata);
function gparquet_file_metadata_get_n_columns(metadata) bind(c)
  import :: c_int, c_ptr
  implicit none
  integer(c_int) :: gparquet_file_metadata_get_n_columns
  type(c_ptr), value :: metadata
end function

! 
!gint gparquet_file_metadata_get_n_schema_elements(GParquetFileMetadata *metadata);
function gparquet_file_metadata_get_n_schema_elements(metadata) bind(c)
  import :: c_int, c_ptr
  implicit none
  integer(c_int) :: gparquet_file_metadata_get_n_schema_elements
  type(c_ptr), value :: metadata
end function

! 
!gint64 gparquet_file_metadata_get_n_rows(GParquetFileMetadata *metadata);
function gparquet_file_metadata_get_n_rows(metadata) bind(c)
  import :: c_int64_t, c_ptr
  implicit none
  integer(c_int64_t) :: gparquet_file_metadata_get_n_rows
  type(c_ptr), value :: metadata
end function

! 
!gint gparquet_file_metadata_get_n_row_groups(GParquetFileMetadata *metadata);
function gparquet_file_metadata_get_n_row_groups(metadata) bind(c)
  import :: c_int, c_ptr
  implicit none
  integer(c_int) :: gparquet_file_metadata_get_n_row_groups
  type(c_ptr), value :: metadata
end function

! 
!GParquetRowGroupMetadata * gparquet_file_metadata_get_row_group(GParquetFileMetadata *metadata, gint index, GError **error);
function gparquet_file_metadata_get_row_group(metadata, index, error) bind(c)
  import :: c_ptr, c_int
  implicit none
  type(c_ptr) :: gparquet_file_metadata_get_row_group
  type(c_ptr), value :: metadata
  integer(c_int), value :: index
  type(c_ptr), value :: error
end function

! 
!const gchar * gparquet_file_metadata_get_created_by(GParquetFileMetadata *metadata);
function gparquet_file_metadata_get_created_by(metadata) bind(c)
  import :: c_ptr
  implicit none
  type(c_ptr) :: gparquet_file_metadata_get_created_by
  type(c_ptr), value :: metadata
end function

! 
!guint32 gparquet_file_metadata_get_size(GParquetFileMetadata *metadata);
function gparquet_file_metadata_get_size(metadata) bind(c)
  import :: c_int32_t, c_ptr
  implicit none
  integer(c_int32_t) :: gparquet_file_metadata_get_size
  type(c_ptr), value :: metadata
end function

! 
!gboolean gparquet_file_metadata_can_decompress(GParquetFileMetadata *metadata);
function gparquet_file_metadata_can_decompress(metadata) bind(c)
  import :: c_int, c_ptr
  implicit none
  integer(c_int) :: gparquet_file_metadata_can_decompress
  type(c_ptr), value :: metadata
end function

!--------------------------------------------------
! /usr/include/parquet-glib/arrow-file-reader.hpp
!--------------------------------------------------
!--------------------------------------------------
! /usr/include/parquet-glib/parquet-glib.hpp
!--------------------------------------------------
!--------------------------------------------------
! /usr/include/parquet-glib/arrow-file-writer.h
!--------------------------------------------------
! GARROW_AVAILABLE_IN_0_17
!GParquetWriterProperties *gparquet_writer_properties_new(void);
function gparquet_writer_properties_new() bind(c)
  import :: c_ptr
  implicit none
  type(c_ptr) :: gparquet_writer_properties_new
end function

! 
!void gparquet_writer_properties_set_compression(GParquetWriterProperties *properties, GArrowCompressionType compression_type, const gchar *path);
subroutine gparquet_writer_properties_set_compression(properties,&
& compression_type, path) bind(c)
  import :: c_ptr, c_int, c_char
  implicit none
  type(c_ptr), value :: properties
  integer(c_int), value :: compression_type
  character(kind=c_char), dimension(*) :: path
end subroutine

! 
!GArrowCompressionType gparquet_writer_properties_get_compression_path(GParquetWriterProperties *properties, const gchar *path);
function gparquet_writer_properties_get_compression_path(properties, path)&
& bind(c)
  import :: c_int, c_ptr, c_char
  implicit none
  integer(c_int) :: gparquet_writer_properties_get_compression_path
  type(c_ptr), value :: properties
  character(kind=c_char), dimension(*) :: path
end function

! 
!void gparquet_writer_properties_enable_dictionary(GParquetWriterProperties *properties, const gchar *path);
subroutine gparquet_writer_properties_enable_dictionary(properties, path)&
& bind(c)
  import :: c_ptr, c_char
  implicit none
  type(c_ptr), value :: properties
  character(kind=c_char), dimension(*) :: path
end subroutine

! 
!void gparquet_writer_properties_disable_dictionary(GParquetWriterProperties *properties, const gchar *path);
subroutine gparquet_writer_properties_disable_dictionary(properties, path)&
& bind(c)
  import :: c_ptr, c_char
  implicit none
  type(c_ptr), value :: properties
  character(kind=c_char), dimension(*) :: path
end subroutine

! 
!gboolean gparquet_writer_properties_is_dictionary_enabled(GParquetWriterProperties *properties, const gchar *path);
function gparquet_writer_properties_is_dictionary_enabled(properties, path)&
& bind(c)
  import :: c_int, c_ptr, c_char
  implicit none
  integer(c_int) :: gparquet_writer_properties_is_dictionary_enabled
  type(c_ptr), value :: properties
  character(kind=c_char), dimension(*) :: path
end function

! 
!void gparquet_writer_properties_set_dictionary_page_size_limit(GParquetWriterProperties *properties, gint64 limit);
subroutine gparquet_writer_properties_set_dictionary_page_size_limit(properties&
&, limit) bind(c)
  import :: c_ptr, c_int64_t
  implicit none
  type(c_ptr), value :: properties
  integer(c_int64_t), value :: limit
end subroutine

! 
!gint64 gparquet_writer_properties_get_dictionary_page_size_limit(GParquetWriterProperties *properties);
function gparquet_writer_properties_get_dictionary_page_size_limit(properties)&
& bind(c)
  import :: c_int64_t, c_ptr
  implicit none
  integer(c_int64_t) :: gparquet_writer_properties_get_dictionary_page_size_limit
  type(c_ptr), value :: properties
end function

! 
!void gparquet_writer_properties_set_batch_size(GParquetWriterProperties *properties, gint64 batch_size);
subroutine gparquet_writer_properties_set_batch_size(properties, batch_size)&
& bind(c)
  import :: c_ptr, c_int64_t
  implicit none
  type(c_ptr), value :: properties
  integer(c_int64_t), value :: batch_size
end subroutine

! 
!gint64 gparquet_writer_properties_get_batch_size(GParquetWriterProperties *properties);
function gparquet_writer_properties_get_batch_size(properties) bind(c)
  import :: c_int64_t, c_ptr
  implicit none
  integer(c_int64_t) :: gparquet_writer_properties_get_batch_size
  type(c_ptr), value :: properties
end function

! 
!void gparquet_writer_properties_set_max_row_group_length(GParquetWriterProperties *properties, gint64 length);
subroutine gparquet_writer_properties_set_max_row_group_length(properties,&
& length) bind(c)
  import :: c_ptr, c_int64_t
  implicit none
  type(c_ptr), value :: properties
  integer(c_int64_t), value :: length
end subroutine

! 
!gint64 gparquet_writer_properties_get_max_row_group_length(GParquetWriterProperties *properties);
function gparquet_writer_properties_get_max_row_group_length(properties)&
& bind(c)
  import :: c_int64_t, c_ptr
  implicit none
  integer(c_int64_t) :: gparquet_writer_properties_get_max_row_group_length
  type(c_ptr), value :: properties
end function

! 
!void gparquet_writer_properties_set_data_page_size(GParquetWriterProperties *properties, gint64 data_page_size);
subroutine gparquet_writer_properties_set_data_page_size(properties,&
& data_page_size) bind(c)
  import :: c_ptr, c_int64_t
  implicit none
  type(c_ptr), value :: properties
  integer(c_int64_t), value :: data_page_size
end subroutine

! 
!gint64 gparquet_writer_properties_get_data_page_size(GParquetWriterProperties *properties);
function gparquet_writer_properties_get_data_page_size(properties) bind(c)
  import :: c_int64_t, c_ptr
  implicit none
  integer(c_int64_t) :: gparquet_writer_properties_get_data_page_size
  type(c_ptr), value :: properties
end function

! 
!GParquetArrowFileWriter * gparquet_arrow_file_writer_new_arrow(GArrowSchema *schema, GArrowOutputStream *sink, GParquetWriterProperties *writer_properties, GError **error);
function gparquet_arrow_file_writer_new_arrow(schema, sink, writer_properties,&
& error) bind(c)
  import :: c_ptr
  implicit none
  type(c_ptr) :: gparquet_arrow_file_writer_new_arrow
  type(c_ptr), value :: schema
  type(c_ptr), value :: sink
  type(c_ptr), value :: writer_properties
  type(c_ptr), value :: error
end function

! 
!GParquetArrowFileWriter * gparquet_arrow_file_writer_new_path(GArrowSchema *schema, const gchar *path, GParquetWriterProperties *writer_properties, GError **error);
function gparquet_arrow_file_writer_new_path(schema, path, writer_properties,&
& error) bind(c)
  import :: c_ptr, c_char
  implicit none
  type(c_ptr) :: gparquet_arrow_file_writer_new_path
  type(c_ptr), value :: schema
  character(kind=c_char), dimension(*) :: path
  type(c_ptr), value :: writer_properties
  type(c_ptr), value :: error
end function

! 
!gboolean gparquet_arrow_file_writer_write_table(GParquetArrowFileWriter *writer, GArrowTable *table, guint64 chunk_size, GError **error);
function gparquet_arrow_file_writer_write_table(writer, table, chunk_size,&
& error) bind(c)
  import :: c_int, c_ptr, c_int64_t
  implicit none
  integer(c_int) :: gparquet_arrow_file_writer_write_table
  type(c_ptr), value :: writer
  type(c_ptr), value :: table
  integer(c_int64_t), value :: chunk_size
  type(c_ptr), value :: error
end function

! 
!gboolean gparquet_arrow_file_writer_close(GParquetArrowFileWriter *writer, GError **error);
function gparquet_arrow_file_writer_close(writer, error) bind(c)
  import :: c_int, c_ptr
  implicit none
  integer(c_int) :: gparquet_arrow_file_writer_close
  type(c_ptr), value :: writer
  type(c_ptr), value :: error
end function

!--------------------------------------------------
! /usr/include/parquet-glib/arrow-file-writer.hpp
!--------------------------------------------------
!--------------------------------------------------
! /usr/include/parquet-glib/statistics.h
!--------------------------------------------------
! 
!gboolean gparquet_statistics_equal(GParquetStatistics *statistics, GParquetStatistics *other_statistics);
function gparquet_statistics_equal(statistics, other_statistics) bind(c)
  import :: c_int, c_ptr
  implicit none
  integer(c_int) :: gparquet_statistics_equal
  type(c_ptr), value :: statistics
  type(c_ptr), value :: other_statistics
end function

! 
!gboolean gparquet_statistics_has_n_nulls(GParquetStatistics *statistics);
function gparquet_statistics_has_n_nulls(statistics) bind(c)
  import :: c_int, c_ptr
  implicit none
  integer(c_int) :: gparquet_statistics_has_n_nulls
  type(c_ptr), value :: statistics
end function

! 
!gint64 gparquet_statistics_get_n_nulls(GParquetStatistics *statistics);
function gparquet_statistics_get_n_nulls(statistics) bind(c)
  import :: c_int64_t, c_ptr
  implicit none
  integer(c_int64_t) :: gparquet_statistics_get_n_nulls
  type(c_ptr), value :: statistics
end function

! 
!gboolean gparquet_statistics_has_n_distinct_values(GParquetStatistics *statistics);
function gparquet_statistics_has_n_distinct_values(statistics) bind(c)
  import :: c_int, c_ptr
  implicit none
  integer(c_int) :: gparquet_statistics_has_n_distinct_values
  type(c_ptr), value :: statistics
end function

! 
!gint64 gparquet_statistics_get_n_distinct_values(GParquetStatistics *statistics);
function gparquet_statistics_get_n_distinct_values(statistics) bind(c)
  import :: c_int64_t, c_ptr
  implicit none
  integer(c_int64_t) :: gparquet_statistics_get_n_distinct_values
  type(c_ptr), value :: statistics
end function

! 
!gint64 gparquet_statistics_get_n_values(GParquetStatistics *statistics);
function gparquet_statistics_get_n_values(statistics) bind(c)
  import :: c_int64_t, c_ptr
  implicit none
  integer(c_int64_t) :: gparquet_statistics_get_n_values
  type(c_ptr), value :: statistics
end function

! 
!gboolean gparquet_statistics_has_min_max(GParquetStatistics *statistics);
function gparquet_statistics_has_min_max(statistics) bind(c)
  import :: c_int, c_ptr
  implicit none
  integer(c_int) :: gparquet_statistics_has_min_max
  type(c_ptr), value :: statistics
end function

! 
!gboolean gparquet_boolean_statistics_get_min(GParquetBooleanStatistics *statistics);
function gparquet_boolean_statistics_get_min(statistics) bind(c)
  import :: c_int, c_ptr
  implicit none
  integer(c_int) :: gparquet_boolean_statistics_get_min
  type(c_ptr), value :: statistics
end function

! 
!gboolean gparquet_boolean_statistics_get_max(GParquetBooleanStatistics *statistics);
function gparquet_boolean_statistics_get_max(statistics) bind(c)
  import :: c_int, c_ptr
  implicit none
  integer(c_int) :: gparquet_boolean_statistics_get_max
  type(c_ptr), value :: statistics
end function

! 
!gint32 gparquet_int32_statistics_get_min(GParquetInt32Statistics *statistics);
function gparquet_int32_statistics_get_min(statistics) bind(c)
  import :: c_int32_t, c_ptr
  implicit none
  integer(c_int32_t) :: gparquet_int32_statistics_get_min
  type(c_ptr), value :: statistics
end function

! 
!gint32 gparquet_int32_statistics_get_max(GParquetInt32Statistics *statistics);
function gparquet_int32_statistics_get_max(statistics) bind(c)
  import :: c_int32_t, c_ptr
  implicit none
  integer(c_int32_t) :: gparquet_int32_statistics_get_max
  type(c_ptr), value :: statistics
end function

! 
!gint64 gparquet_int64_statistics_get_min(GParquetInt64Statistics *statistics);
function gparquet_int64_statistics_get_min(statistics) bind(c)
  import :: c_int64_t, c_ptr
  implicit none
  integer(c_int64_t) :: gparquet_int64_statistics_get_min
  type(c_ptr), value :: statistics
end function

! 
!gint64 gparquet_int64_statistics_get_max(GParquetInt64Statistics *statistics);
function gparquet_int64_statistics_get_max(statistics) bind(c)
  import :: c_int64_t, c_ptr
  implicit none
  integer(c_int64_t) :: gparquet_int64_statistics_get_max
  type(c_ptr), value :: statistics
end function

! 
!gfloat gparquet_float_statistics_get_min(GParquetFloatStatistics *statistics);
function gparquet_float_statistics_get_min(statistics) bind(c)
  import :: c_float, c_ptr
  implicit none
  real(c_float) :: gparquet_float_statistics_get_min
  type(c_ptr), value :: statistics
end function

! 
!gfloat gparquet_float_statistics_get_max(GParquetFloatStatistics *statistics);
function gparquet_float_statistics_get_max(statistics) bind(c)
  import :: c_float, c_ptr
  implicit none
  real(c_float) :: gparquet_float_statistics_get_max
  type(c_ptr), value :: statistics
end function

! 
!gdouble gparquet_double_statistics_get_min(GParquetDoubleStatistics *statistics);
function gparquet_double_statistics_get_min(statistics) bind(c)
  import :: c_double, c_ptr
  implicit none
  real(c_double) :: gparquet_double_statistics_get_min
  type(c_ptr), value :: statistics
end function

! 
!gdouble gparquet_double_statistics_get_max(GParquetDoubleStatistics *statistics);
function gparquet_double_statistics_get_max(statistics) bind(c)
  import :: c_double, c_ptr
  implicit none
  real(c_double) :: gparquet_double_statistics_get_max
  type(c_ptr), value :: statistics
end function

! 
!GBytes * gparquet_byte_array_statistics_get_min(GParquetByteArrayStatistics *statistics);
function gparquet_byte_array_statistics_get_min(statistics) bind(c)
  import :: c_ptr
  implicit none
  type(c_ptr) :: gparquet_byte_array_statistics_get_min
  type(c_ptr), value :: statistics
end function

! 
!GBytes * gparquet_byte_array_statistics_get_max(GParquetByteArrayStatistics *statistics);
function gparquet_byte_array_statistics_get_max(statistics) bind(c)
  import :: c_ptr
  implicit none
  type(c_ptr) :: gparquet_byte_array_statistics_get_max
  type(c_ptr), value :: statistics
end function

! 
!GBytes * gparquet_fixed_length_byte_array_statistics_get_min( GParquetFixedLengthByteArrayStatistics *statistics);
function gparquet_fixed_length_byte_array_statistics_get_min(statistics)&
& bind(c)
  import :: c_ptr
  implicit none
  type(c_ptr) :: gparquet_fixed_length_byte_array_statistics_get_min
  type(c_ptr), value :: statistics
end function

! 
!GBytes * gparquet_fixed_length_byte_array_statistics_get_max( GParquetFixedLengthByteArrayStatistics *statistics);
function gparquet_fixed_length_byte_array_statistics_get_max(statistics)&
& bind(c)
  import :: c_ptr
  implicit none
  type(c_ptr) :: gparquet_fixed_length_byte_array_statistics_get_max
  type(c_ptr), value :: statistics
end function

!--------------------------------------------------
! /usr/include/parquet-glib/arrow-file-reader.h
!--------------------------------------------------
! 
!GParquetArrowFileReader * gparquet_arrow_file_reader_new_arrow(GArrowSeekableInputStream *source, GError **error);
function gparquet_arrow_file_reader_new_arrow(source, error) bind(c)
  import :: c_ptr
  implicit none
  type(c_ptr) :: gparquet_arrow_file_reader_new_arrow
  type(c_ptr), value :: source
  type(c_ptr), value :: error
end function

! 
!GParquetArrowFileReader * gparquet_arrow_file_reader_new_path(const gchar *path, GError **error);
function gparquet_arrow_file_reader_new_path(path, error) bind(c)
  import :: c_ptr, c_char
  implicit none
  type(c_ptr) :: gparquet_arrow_file_reader_new_path
  character(kind=c_char), dimension(*) :: path
  type(c_ptr), value :: error
end function

! 
!GArrowTable * gparquet_arrow_file_reader_read_table(GParquetArrowFileReader *reader, GError **error);
function gparquet_arrow_file_reader_read_table(reader, error) bind(c)
  import :: c_ptr
  implicit none
  type(c_ptr) :: gparquet_arrow_file_reader_read_table
  type(c_ptr), value :: reader
  type(c_ptr), value :: error
end function

! 
!GArrowTable * gparquet_arrow_file_reader_read_row_group(GParquetArrowFileReader *reader, gint row_group_index, gint *column_indices, gsize n_column_indices, GError **error);
function gparquet_arrow_file_reader_read_row_group(reader, row_group_index,&
& column_indices, n_column_indices, error) bind(c)
  import :: c_ptr, c_int, c_size_t
  implicit none
  type(c_ptr) :: gparquet_arrow_file_reader_read_row_group
  type(c_ptr), value :: reader
  integer(c_int), value :: row_group_index
  type(c_ptr), value :: column_indices
  integer(c_size_t), value :: n_column_indices
  type(c_ptr), value :: error
end function

! 
!GArrowSchema * gparquet_arrow_file_reader_get_schema(GParquetArrowFileReader *reader, GError **error);
function gparquet_arrow_file_reader_get_schema(reader, error) bind(c)
  import :: c_ptr
  implicit none
  type(c_ptr) :: gparquet_arrow_file_reader_get_schema
  type(c_ptr), value :: reader
  type(c_ptr), value :: error
end function

! 
!GArrowChunkedArray * gparquet_arrow_file_reader_read_column_data(GParquetArrowFileReader *reader, gint i, GError **error);
function gparquet_arrow_file_reader_read_column_data(reader, i, error) bind(c)
  import :: c_ptr, c_int
  implicit none
  type(c_ptr) :: gparquet_arrow_file_reader_read_column_data
  type(c_ptr), value :: reader
  integer(c_int), value :: i
  type(c_ptr), value :: error
end function

! 
!gint gparquet_arrow_file_reader_get_n_row_groups(GParquetArrowFileReader *reader);
function gparquet_arrow_file_reader_get_n_row_groups(reader) bind(c)
  import :: c_int, c_ptr
  implicit none
  integer(c_int) :: gparquet_arrow_file_reader_get_n_row_groups
  type(c_ptr), value :: reader
end function

! 
!gint64 gparquet_arrow_file_reader_get_n_rows(GParquetArrowFileReader *reader);
function gparquet_arrow_file_reader_get_n_rows(reader) bind(c)
  import :: c_int64_t, c_ptr
  implicit none
  integer(c_int64_t) :: gparquet_arrow_file_reader_get_n_rows
  type(c_ptr), value :: reader
end function

! 
!void gparquet_arrow_file_reader_set_use_threads(GParquetArrowFileReader *reader, gboolean use_threads);
subroutine gparquet_arrow_file_reader_set_use_threads(reader, use_threads)&
& bind(c)
  import :: c_ptr, c_int
  implicit none
  type(c_ptr), value :: reader
  integer(c_int), value :: use_threads
end subroutine

! 
!GParquetFileMetadata * gparquet_arrow_file_reader_get_metadata(GParquetArrowFileReader *reader);
function gparquet_arrow_file_reader_get_metadata(reader) bind(c)
  import :: c_ptr
  implicit none
  type(c_ptr) :: gparquet_arrow_file_reader_get_metadata
  type(c_ptr), value :: reader
end function

end interface
end module parquet_glib
