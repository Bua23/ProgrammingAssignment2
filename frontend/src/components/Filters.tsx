interface Props {
  search: string;
  onSearchChange: (v: string) => void;
  company: string;
  onCompanyChange: (v: string) => void;
  companies: string[];
  role: string;
  onRoleChange: (v: string) => void;
  roles: [string, string][]; // [key, label]
}

export function Filters({
  search,
  onSearchChange,
  company,
  onCompanyChange,
  companies,
  role,
  onRoleChange,
  roles,
}: Props) {
  return (
    <div className="filters">
      <input
        type="text"
        placeholder="Search title, company, or description…"
        value={search}
        onChange={(e) => onSearchChange(e.target.value)}
        aria-label="Search jobs"
      />
      {roles.length > 0 && (
        <select
          value={role}
          onChange={(e) => onRoleChange(e.target.value)}
          aria-label="Filter by role family"
        >
          <option value="">All role families</option>
          {roles.map(([key, label]) => (
            <option key={key} value={key}>
              {label}
            </option>
          ))}
        </select>
      )}
      <select
        value={company}
        onChange={(e) => onCompanyChange(e.target.value)}
        aria-label="Filter by company"
      >
        <option value="">All companies</option>
        {companies.map((c) => (
          <option key={c} value={c}>
            {c}
          </option>
        ))}
      </select>
    </div>
  );
}
