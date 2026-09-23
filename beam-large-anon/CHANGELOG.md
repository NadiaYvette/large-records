-- | Changelog for beam-large-anon

## 0.1.0 (2026-09-23)

- Initial release.
- Provides 'AnonTable', a newtype wrapper allowing 'Record f r' anonymous
  records (from large-anon) to be used as beam tables.
- Implements 'Beamable', 'tblSkeleton', 'autoDbSettings', 'FromBackendRow',
  and table lenses via the large-anon generic machinery, without GHC.Generics.
